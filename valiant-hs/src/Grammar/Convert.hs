module Convert where

begin notes:

A ::= A B | C D E | E | f P G A A

rewrite to rules

A_1 -> A B
A_2 -> C D E
A_3 -> E
A_4 -> f P G A A

rewrite rules with too many terms on RHS

A_1 -> A B                              -- unchanged
A_2_1 -> A_2_2 E                        -- split into two rules
A_2_2 -> C D
A_3 -> E                                -- unchanged
A_4 -> f P G A A                        -- multi-step rewrite

-- split in half
f P G , A A                             -- divide and conquer.  The ideal
                                        -- position to divide the list is
                                        -- wherever minimizes subtrees
                                        -- Generally this is halving the list
A_4_0 = A_4_1 A_4_2                     -- two sub-rules
A_4_1 = f P G
A_4_2 = A A


-- A_4_1 is still too long
A_4_1_1 = A_4_1_2 G                     -- split the intuitive way
A_4_1_2 = f P
A_4_2 = A A


finally we have the following, with no more than
2 terms on the right hand side of any rule

A_1 -> A B
A_2 -> A_2_1
A_2_1 -> A_2_2 E
A_2_2 -> C D
A_3 -> E
A_4 -> A_4_1_1
A_4_1_1 = A_4_1_2 G
A_4_1_2 = f P
A_4_2 = A A

now we must rewrite single-term rules

A_1 -> A B
A_2 -> A_2_1                            -- this
A_2_1 -> A_2_2 E
A_2_2 -> C D
A_3 -> E                                -- this
A_4 -> A_4_1_1                          -- this
A_4_1_1 = A_4_1_2 G
A_4_1_2 = f P
A_4_2 = A A

A_2 -> A_2_1
A_2_1 -> A_2_2 E
-- can simply be lifted to
A_2 -> A_2_2 E

A_3 -> E
-- idk

A_4 -> A_4_1_1
A_4_1_1 -> A_4_1_2 G
-- again lift
A_4 -> A_4_1_2 G


things to consider

A ::= B (C | D) E
must expand to
A -> B C E
A -> B D E
and then
A -> B A2
A2 -> C E
A -> B A3
A3 -> D E
