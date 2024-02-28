# Introducing SWISH DataLab

The SWISH DataLab addresses one of the main bottlenecks of data science,
bringing data from different sources together, cleaning and selecting
this data. Most pipelines use a general purpose programming language
such as Python to clean and ingest the data into a linked data store or
RDBMS after which the relevant data is selected and applicable machine
learning is applied. In contrast, SWISH data management is based on
Prolog, a _relational_ and _logic_ based language. External data sources
such as RDBMS systems, Linked Data, CSV files, XML files, JSON, etc. are
made available using a mixture of _adaptors_ that make the data
available in Prolog's relational model without transferring the data and
_ingestion_, which loads the data into Prolog.

Subsequently, declarative rules are stated to define a clean and
coherent view on the data that is targetted towards analysing this data.
Due to the logic basis of Prolog this view is modular, concise and
declarative, making it easy to maintain. SWI-Prolog's _tabling_
extension provides the same termination properties as DataLog as well as
the same order indepency of rules within the subset Prolog shares with
DataLog. Tabling also provides _caching_ results. At the same time,
users have access to the more general Prolog language to code
transformations that are not supported by DataLog.

SWISH unites [SWI-Prolog](https://www.swi-prolog.org) and
[R](https://www.r-project.org/) together behind a web based IDE that
resembles [Jupyter](https://jupyter.org/) notebooks. This platform can
be deployed on your laptop as well as on a server. The platform allows
multiple data scientists to work on the same data simultaneously while
rule sets can be reused and shared between users. This notably allows
technical people to provide more complicated data transformation steps
to domain experts. The platform can be configured to allow both
authenticated users and anonymous users with limited access rights.
Notebooks and programs are stored in a GIT-like repository and fully
versioned. It is possible to create a snapshot of a query and all
relevant programs for reliable reproduction of results. Data views
defined in SWISH may be downloaded as CSV and can be accessed through a
web based API.

Using Prolog for data integration, cleaning and modelling started life
as a valorisation project within [COMMIT/](https://www.commit-nl.nl/). A
web enabled version of SWI-Prolog was pioneered by [Torbjörn
Lager](https://www.gu.se/english/about_the_university/staff/?languageId=100001&userId=xlagto)
The combination of Prolog and R has been pioneered by Nicos Angelopoulos
at the NKI (Dutch Cancer Institute) in the life sciences domain. SWISH
is in use at CWI to analyse user behaviour based on HTTP log data from
the Dutch national library (Koninklijke Bibliotheek). Samer Abdallah
(University College London) uses SWISH for analysing music. The core of
SWISH is under active development and heavily tested as a shared Prolog
teaching environment.

Useful links:

  - [Download SWISH from GitHub](https://github.com/SWI-Prolog/swish)
  - [SWISH and R for Docker](https://hub.docker.com/u/swipl)
  - [SWISH for Prolog teaching](https://swish.swi-prolog.org)
  - [SWISH DataLab: A Web Interface for Data Exploration and Analysis,
     BNAIC 2016](https://www.springerprofessional.de/en/swish-datalab-a-web-interface-for-data-exploration-and-analysis/15059986)



