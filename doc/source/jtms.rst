Justification Based Truth Maintenance System
============================================

Justification Based Truth Maintenance System or JTMS for short

API
---

``create-jtms`` create instance of JTMS

``create-node`` create node in TMS

``assume-node`` make node an assumption and enable it

``enable-assumption`` turn assumption ``'in``

``retract-assumption`` turn assumption ``'out``

``justify-node`` justify a node with justification

``in-node?`` check if node is ``'in``

``out-node?`` check if node is ``'out``

``assumptions-of-node`` list all assumptions and premises having effect to this node

``supporting-justification-for-node`` which justification is currently supporting a node

