Testing if/then/else
  $ electrod --pg -v $TESTDIR/ifthenelse.elo
  electrod* (glob)
  Processing file: .* (re)
  [INFO]* (glob)
  [INFO]* (glob)
  [INFO]* (glob)
  [INFO]* (glob)
  Generated file:
  -- Generated* (glob)
  MODULE main
  JUSTICE TRUE;
  
  
  -- {((e1 in rel)) implies
  --    ((s in none))
  --  else
  --    (((e2 in rel)) implies
  --       ((some some/12 : s {true}))
  --     else
  --       (((e3 in rel) implies (some some/13 : s {true}))))
  -- }
  INVAR
  ((rel$a1 -> !s$a) &
     (!rel$a1 -> ((rel$a2 -> s$a) & (!rel$a2 -> (rel$a3 -> s$a))))
  );
  
  -- {((e1 in rel)) implies
  --    ((s in none))
  --  else
  --    (((e2 in rel)) implies
  --       ((some some/10 : s {true}))
  --     else
  --       (((e3 in rel) implies (some some/11 : s {true}))))
  -- }
  INVAR
  ((rel$a1 -> !s$a) &
     (!rel$a1 -> ((rel$a2 -> s$a) & (!rel$a2 -> (rel$a3 -> s$a))))
  );
  
  -- {(((e1 in rel)) implies ((s in none)) else ((e2 in rel)) implies
  --    (some some/9 : s {true}))
  -- }
  INVAR
  (((rel$a1 -> !s$a) & (!rel$a1 -> rel$a2)) -> s$a);
  
  -- {((e1 in rel)) implies
  --    ((s in none))
  --  else
  --    (((e2 in rel) implies (some some/8 : s {true})))
  -- }
  INVAR
  ((rel$a1 -> !s$a) & (!rel$a1 -> (rel$a2 -> s$a)));
  
  -- {((e1 in rel)) implies
  --    ((s in none))
  --  else
  --    (((e2 in rel) implies (some some/7 : s {true})))
  -- }
  INVAR
  ((rel$a1 -> !s$a) & (!rel$a1 -> (rel$a2 -> s$a)));
  
  -- {((e1 in rel)) implies
  --    ((s in none))
  --  else
  --    (((e2 in rel) iff (some some/6 : s {true})))
  -- }
  INVAR
  ((rel$a1 -> !s$a) & (!rel$a1 -> (rel$a2 <-> s$a)));
  
  -- {(((e1 in rel)) implies ((s in none)) else ((e2 in rel)) iff
  --    (some some/5 : s {true}))
  -- }
  INVAR
  (((rel$a1 -> !s$a) & (!rel$a1 -> rel$a2)) <-> s$a);
  
  -- {(((e1 in rel)) implies ((s in none)) else ((e2 in rel)) iff
  --    (some some/4 : s {true}))
  -- }
  INVAR
  (((rel$a1 -> !s$a) & (!rel$a1 -> rel$a2)) <-> s$a);
  
  -- {(((e1 in rel)) implies ((s in none)) else ((e2 in rel)) and
  --    (some some/3 : s {true}))
  -- }
  INVAR
  (((rel$a1 -> !s$a) & (!rel$a1 -> rel$a2)) & s$a);
  
  -- {((e1 in rel)) implies
  --    ((s in none))
  --  else
  --    (((e2 in rel) and (some some/2 : s {true})))
  -- }
  INVAR
  ((rel$a1 -> !s$a) & (!rel$a1 -> (rel$a2 & s$a)));
  
  -- {((e1 in rel)) implies
  --    ((s in none))
  --  else
  --    (((e2 in rel) and (some some/1 : s {true})))
  -- }
  INVAR
  ((rel$a1 -> !s$a) & (!rel$a1 -> (rel$a2 & s$a)));
  
  -- {((e1 in rel) implies ((e2 in rel) implies (some some/0 : s {true})))}
  INVAR
  (rel$a1 -> (rel$a2 -> s$a));
  
  
  
  -- {((rel = e1)) implies
  --    ((e1 in r'))
  --  else
  --    (((rel = e2)) implies ((e2 in r')) else (((rel = e3) implies (e3 in r'))))
  -- }
  TRANS
  (((rel$a1 & (!rel$a2 & !rel$a3)) -> next(r$a1)) &
     ((rel$a1 -> (rel$a2 | rel$a3)) ->
        (((rel$a2 & (!rel$a1 & !rel$a3)) -> next(r$a2)) &
           ((rel$a2 -> (rel$a1 | rel$a3)) ->
              ((rel$a3 & (!rel$a1 & !rel$a2)) -> next(r$a3))
           )
        )
     )
  );
  
  
  -- (not (sometime (some some/14 : s {true})))
  LTLSPEC
  !(F s$a);
  
  
  VAR s$a : boolean;
  VAR rel$a1 : boolean;
  VAR rel$a2 : boolean;
  VAR rel$a3 : boolean;
  VAR r$a1 : boolean;
  VAR r$a2 : boolean;
  VAR r$a3 : boolean;
  
  
  [INFO] * (glob)
  [INFO] * (glob)
         * (glob)
  [INFO] * (glob)
  [INFO] * (glob)
   e1  | {a1}    | -==- | -==-
  -----+---------+------+---------
   e2  | {a2}    | -==- | -==-
  -----+---------+------+---------
   e3  | {a3}    | -==- | -==-
  -----+---------+------+---------
   r   | {}      | -==- | {a2}
  -----+---------+------+---------
   rel | {a2 a3} | {a2} | {a2 a3}
  -----+---------+------+---------
   s   | {a}     | -==- | -==-
  -----+---------+------+---------
       | LOOP    |      |  
  [INFO] Total * (glob)
  Elapsed * (glob)
