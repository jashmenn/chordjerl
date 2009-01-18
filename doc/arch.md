Disclaimer
==========
this file is just a scratch pad for pre-alpha thoughts. most of this will
probably change as we get into implementation. suggestions are welcome

Intro
============
Each machine contains a whole chord server. A chord server consists of these parts:

/*ch_node_controller_sup*/
/*==========================*/
/*supervises the node controller. nothing else*/

ch_node_controller
===============
negotiates communication between nodes. 
  * creates and supervises all (virtual) nodes
  * assigns local node numbers

ch_node
===============
all nodes are virtual. If you only want one node, then set your number of
virtual nodes to one.

ch_config
===============
  * stores the module that provides the backing store

gen_ch_backing_store (behavior)
================
  * specifies the interface to be adheared to for the backing store


chordjerl_server
================
registered process, everything connects to here.  



  * who stores the whole server's ip?
