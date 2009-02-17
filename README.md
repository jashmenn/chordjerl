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

Here's a simple network where each node has 1 finger. Dotted lines denote predecessors. (m=7)

![nodes!](http://github.com/jashmenn/chordjerl/tree/master/doc/simple_nodes.png?raw=true)
