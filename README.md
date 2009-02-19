Chordjerl
=========

The goal of Chordjerl (pronounced: cordial) is to be an reusable Erlang
implementation of the Chord distrubuted key lookup protocol. 

Chord
=====

Chord is a lookup protocol for efficient location of a node that stores a
desired data item in a decentralized, peer-to-peer system. The Chord algorithm provides the foundation for a reliable, efficient, scalable decentralized distributed hash.

Chord provides support for just one operation: given a key, it maps the key
onto a node.

You can read more about Chord in the paper from Stoica et al. "Chord: A
Scalable Peer-to-peer Lookup Protocol for Internet Applications" (http://pdos.csail.mit.edu/papers/ton:chord/paper-ton.pdf)

Status
=======

Pre-Alpha 

Integration
===========

Chordjerl is designed to work with Ari Lerner's `layers` <http://github.com/auser/layers/tree/master> and `converse` <http://github.com/auser/converse/tree/master>


Screenshots
===========

Here's a simple network of 7 nodes where m (the number of bits in the hash function) = 7

![nodes!](http://github.com/jashmenn/chordjerl/tree/master/doc/server_m-7_7_nodes.png?raw=true)

Here's a a more complex, but stable, network of 20 nodes with m=15

![nodes!](http://github.com/jashmenn/chordjerl/tree/master/doc/server_m-15_20-nodes-low.gif?raw=true)
