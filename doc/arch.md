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

acts as the node controller and negotiates communication between nodes. 
  * creates and supervises all (virtual) nodes
  * assigns local node numbers
(should the node controller be its own module?)

ch_node
===============
All nodes are "virtual". If you only want one node, then set your number of
virtual nodes to one.
  * has local node number
  * id = SHA(server.ip + local node number) 
  * has finger table

gen_ch_backing_store (behavior)
================
  * specifies the interface to be adheared to for the backing store
