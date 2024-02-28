# Clustered SWISH

## Syncing the gitty store

The gitty store is a directed graph of commits. Each commit is linked to
a _data object_. Both commits and data objects are hashed by content and
read-only. This implies they are easily replicated over the network. The
replication takes two forms:

  - A node may _announce_ an object by sending the objects content as
    a series of chunks.
  - A node may _request_ for an entire object or a missing object
    chunk.  Receiving nodes that have the object will broadcast the
    missing object.

The real problem is updating  the  _head   pointer_.  This  is a central
database that defines the latest version of  a file with a certain name.
This notion must be syncronised.  This is implemented as follows:

  - A node asks the cluster for their current head.
  - If all nodes agree on the current head we are done, but some
    nodes may not have the indicated file.
    - If some nodes have no head, _announce_ the head
  - Else
    - Ask all nodes to produce a backward path of commits that
      includes all reported heads from the other nodes.
    - Work out the last common hash, possibly by majority vote.
    - Work out the changes since this common hash.
      - If nodes agree or have no info, fine
      - If nodes disagree, go with the majority.
    - Propose the new head to all nodes that agreed on the majority
      path.  These nodes will _accept_ if nothing changed since their
      report, blocking further changes for a specified time.
    - If all accept, send a new head notion.  Else restart from the
      beginning.

The  above  deals  with  a  life  cluster.  Nodes  that  have  missed  a
conversation or joined the network later may   miss a file or the latest
version of a file.

## Remote syncing

Remote syncing is necessary for both new cluster members and for cluster
members that have been offline for some time.

  - Find the node with most changes using a request.
  - Ask this node to start the process.
  - Each cluster member checks it has the change.  If not, it starts
    a negotiation using gitty_remote_head/2.

## Profile management and login

FIXME

Remote sync of library(persistency)?

  - Realise a distributed ledger of changes.
  - Apply these.


  - Add serial to each event
  - Broadcast them
  - Adding an event
    - Propos


## Email notifications

FIXME

## Chat subsystem

### Maintain a global overview of visitor count

Visitor change messages cary a `local_visitors` and `visitors` field and
are relayed. Nodes receiving such a message uses the `local_visitors` to
update their count of visitors on  that   node.  Nodes  composing such a
message count the local visitors and add the known totals from the other
nodes.

### Subscribed files

WSID joining a file, leaving a file   or  logging out is broadcasted and
each node maintains a view of the remote users by WSID.

FIXME: need to deal with joining nodes and missed updates.

### Profile changes

Profile changes, login, logout are sent  to   all  nodes  and each nodes
sends them to the browsers that have the WSID watching some file.

### Chat syncing

  - Find the last message of all nodes for DocID.
    - If Serial-ID matches, we are done
    - Else
      - Ask each node for the history as chat(Serial,ID,Time) triples.
      - Asses agreement (= no info or same)
      - If all agree, send an sync request for the serial range that
        is not known everywhere.
      - Else, send an agreement _serial_ and a list of Serial-ID
        pairs constructed from a chronologically ordered list of
	chat messages about which there is no agreement.
