Disclaimer
==========
this file is just a scratch pad for pre-alpha thoughts. most of this will
probably change as we get into implementation. suggestions are welcome

Intro
============
Each machine contains a whole chord server. A chord server consists of these parts:

chordjerl_server_sup
================
registered process. supervises the server

chordjerl_server
================
registered process, everything connects to here. stores the whole server's ip

stores basic configuration options such as:
 * the module that provides the backing store
 * the number of nodes we want
 * other config options

ch_node_controller
===============
negotiates communication between nodes. 
  * creates and supervises all (virtual) nodes
  * assigns local node numbers

(maybe the server should just be the node controller)

ch_node
===============
  * all nodes are virtual. If you only want one node, then set your number of
    virtual nodes to one.
  * has number, id
  * has finger table

gen_ch_backing_store (behavior)
================
  * specifies the interface to be adheared to for the backing store

chordjerl_client
================
code to connect to a node??

