Disclaimer
==========
this file is just a scratch pad for pre-alpha thoughts. most of this will
probably change as we get into implementation. suggestions are welcome

What if we go the simplfy route? Get rid of the virtual nodes and middle
server. Everything is just a node.

Intro
============
Each machine contains a whole chord server. A chord server consists of these parts:

chordjerl_server_sup
================
registered process. supervises 

ch_node
===============
everything connects to here. stores the whole server's ip

 basic configuration options such as:
 * the module that provides the backing store

  * id = SHA(server.ip + local node number) 
  * has finger table

gen_ch_backing_store (behavior)
================
  * specifies the interface to be adheared to for the backing store
