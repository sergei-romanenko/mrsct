MRSC is a toolkit for rapid development and implementation of multi-result supercompilers.

<https://github.com/ilya-klyuchnikov/mrsc>

MRSCT is a variation of MRSC whose purpose is to try a design
based on a combination of _traits + self-types_.

The design of MRSC, at some places, is based on generating "commands"
represented as first-order entities, which are then have to be
executed by explicit interpreters (each interpreter being a monolitic
match-expression.

In MRSCT the generation of "commands" is replaced with direct calls to
methods implementing the "commands".

The purposes of the design based on traits + self-types are the following.

  * Interfaces should be separated from their implementations.

  * There should be possible to have several implementations for an interface.

  * The interfaces should be "opaques" in the following sense. Schematically
    speaking, MRSC is based on a hierarchy of levels: L<sub>0</sub>,
    L<sub>1</sub>, ..., where the operations at a level L<sub>k+1</sub> are
    definded in terms of some operations defined at the level L<sub>k</sub>.
    It is desirable that the operations defined at the level L<sub>k-1</sub>
    be invisible at the level L<sub>k+1</sub>. In other words, the level
    L<sub>k+1</sub> should be only "aware" of the level L<sub>k</sub>,
    but not of L<sub>k-1</sub>.
