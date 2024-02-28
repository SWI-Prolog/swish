# Running SWISH for large user communities

By default,  SWISH uses the local  filesystem to store its  data.  All
data  is stored  in  a directory  `data`.  Data  such  as chats,  user
profile, etc  are stored using SWI-Prolog's  `library(persistency)` as
Prolog  terms.  Files  are saved  in _gitty_  format as  files to  the
subdirectory `data`.  The _gitty_ format  is based on GIT, saving data
based on  the SHA1 hash of  the content and linking  versions of files
together as _commits_.  This format only allows a single SWISH server.

## Using Redis

Alternatively, a [redis](https://redis.io/) database  can be used that
stores the  chats, user profile  data and  the _HEAD_ commit  for each
file.  The  files themselves  are still  stored in  the `data/storage`
directory using the  same format.  This setup uses  Redis _streams_ to
broadcast events of interest to all SWISH nodes in the cluster.  These
events are  used to  make chat  work over the  cluster members  and to
replicate new objects for the `storage` directory:

  - If an object (hashed document) is added to the store, the cluster
    is informed.  Each cluster member stores the object.
  - If a cluster member is down when the object is added, it will
    find the most recent HEAD commit hash from the Redis DB.  If
	the referenced object is not in its store, it broadcasts a
	_discovery request_ for the hash.   The cluster member that
	has a copy of this document will repost it.

To deploy  multiple SWISH instances  using this setup, one  must first
setup a Redis DB.  Currently SWI-Prolog's redis library supports three
of the four redis configurations:

  - Single node.  This is easy, but vulnerable to data loss.  The
    single node also easily becomes a bottleneck.
  - Single node with static replicators.   This avoids data loss.
    It also allows SWISH instances to configure Redis write operations
	to use the master and read operations to use a nearby replicator.
    No files can be saved if the master goes down.
  - High availability cluster.   In this case a set of redis nodes are
    monitored by a set of _sentinels_, minimally 3.  The network
	operates as above, but if the sentinels discover that the master
	is down, they elect a new master and reconfigure the network.
	The SWISH client asks the sentinels for the current configuration.
	If the client gets Redis errors it will reconsult the sentinels and
	reconnect to the possibly changed configuration.

### Getting the Redis network up

Getting the cluster up consists of these steps:

  1. Decide on the Redis configuration to use and configure the
     Redis Db (cluster).
  2. Select a matching SWISH Redis configuration, copy it from
     `config-available` to `config-enabled` and edit to suit
     your setup.  Available configurations:

	   - `config-available/redis_simple.pl` <br>
         Simple single node clear-text connection.   Only use on trusted
         networks!
       - `config-available/redis_sentinel.pl`
         Sentinel cluster using TLS for establishing secure connections.
  3. Bring up SWISH
  4. If you have old data, you may use `lib/redis_transfer.pl` to transfer
     the existing data from the Prolog .db files to populate the Redis DB.
  5. Add new nodes.  Make sure to edit the _Redis consumer_ in each
     enabled configuration.

After  the above,  we have  multiple  SWISH instances  that share  the
files, user  profiles, chat  and HTTP  session management.   Each node
needs to be contacted at its own address though.

## Making the nodes operate as a single service

To make the nodes accessible as a  single service we need some form of
session aware  load balancing.  There are  many ways to do  this.  The
public  site  using  an  [nginx](https://www.nginx.com/)  instance  as
_reverse proxy_.  The nginx _upstream_ mechanism with policy `ip_hash`
is used  for load balancing.   The skeleton setup is  below.  Details,
such as error pages, HTTPS configuration, etc. are left out.

```
upstream swish {
    ip_hash;
    server <url1>
    server <url2>
	...
}

server {
        location / {
                proxy_pass https://swish;
                proxy_http_version 1.1;
                proxy_buffering off;
                client_body_buffer_size 100k;
                proxy_cache off;
                proxy_set_header Host $host
                proxy_set_header X-Real-IP $remote_addr;
                proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                proxy_read_timeout 86400;
        }

        location /chat {
                proxy_pass https://swish;
                proxy_http_version 1.1;
                proxy_set_header Host $host
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
                proxy_set_header X-Real-IP $remote_addr;
                proxy_read_timeout 86400;
        }
}
```

## Many files need better search: enable Elastic search

File search in the file tab  and top-right search box by default using
simple Prolog search.  This works great  with a few hundreds of files,
but above it gets too expensive to walk over each file HEAD commit and
possibly its content.

For  this purpose  we  provided an  [Elastic](https://www.elastic.co/)
plugin.  The setup works as follows:

  1. Launch Elastic
  2. Copy `config-available/elastic.pl` to `config-enabled` and edit
     the location of the Elastic instance and the connection details
     (password, certificates).
  3. Use `lib/plugin/es_swish.pl` (loaded by `config-enabled/elastic.pl`) to
     1. Create the Elastic index using `?- es_create_index.`
	 2. Run `?- es_add(0, 1 000 000).` to populate the index.  The
		arguments are offset and limit, so you can do the job in
		batches to see how it goes.

After enabling,  new documents  are automatically  added to  the index
when they are saved.  If there has been a disruption or you can update
the index with all documents added or modified using `es_add_since/1`,
which takes the number of seconds  to look back.  So, to add documents
for the past week, use:

    ?- es_add_since(7*24*3600).

## Deployment hints

The public instance runs on docker images created using the Dockerfile
from the GIT repo below.

    - https://github.com/SWI-Prolog/docker-swish-public.git

The docker version deploys the  `libssh` pack that allows logging into
the running SWISH server using SSH.   This is used notably for running
the maintenance commands above.

We  maintain the  `config-enabled` directory  of  each node  as a  git
repository that is  a clone of a version maintain  on the machine from
which we  control all instances.  The  version at the node  is checked
out on  a branch  with a  commit that  reflects the  local differences
(consumer in `redis.pl` and the public network address in `network.pl`
if  each server  can  also  be accessed  explicitly).   To update  the
configuration we

  1. Edit it on the maintenance machine
  2. Run `git push <remote> master` to update the remote master
  3. On the node run `git rebase master` to update the node config
Do a  4. Restart SWISH.
