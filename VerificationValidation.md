(This text is my reply to an email question in 2011 from a physicist
about how to use the words "validation" and "verification" when
talking about numerical software.)

#Background:

In the ideal case implementation of software starts from an
unambiguous (formal) specification of the problem to be solved. Then
the implementation is "just" a matter of refining this specification
step by step into something efficiently executable. In practice the
specification is often 1) vague, 2) discovered during implementation
and 3) changing over time as requirements change.

Definitions:
* Verification = evaluate whether or not an implementation satisfies a specification
* Validation = make sure that the specification matches the intended requirements

There are often several "levels of abstraction" where the verification
at a high level corresponds to validation at the next lower level.
This means that the words "validation" and "verification" are
sometimes used interchangeably (and similarly "specification" and
"implementation") which can be confusing unless the level is spelled
out. As an example we could have a case where a PDE + boundary
conditions is a specification at a high level (say level 1), then a
discretised matrix model is the next level (2), a matlab
implementation the third level and a fortran code the lowest level
(4).

We can see the matlab code (3) as an implementation with the
mathematical matrix model (2) as a specification.
Or we can see the matlab code (3) as a specification with the fortran
code as the implementation (4).

Similarly the PDE (1) is a specification for the discretised version
(2), but also an "implementation" of a physical model (at level 0).

http://en.wikipedia.org/wiki/Model-based_testing
