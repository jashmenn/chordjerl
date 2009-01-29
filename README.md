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

Planning stages

(Planned) Features
========

 * Virtual Nodes
 * Behavior module to help implement the value store
 * Optional extensions to routing algorithm/finger table such as node uptime,
   network latency, etc.
 * Support for LAN or WAN use
 

