open Goal
open Types

let%expect_test "appendo: trace" =
  let open Examples in
  print_trace Eval_trace.run [ appendo ] (-1)
    (fresh [ "q" ] @@ invoke "appendo" [ list12; list3; Var "q" ]);
  [%expect
    {|
    ----------------------------------------------

    Step 0:
      <
        fresh (q) appendo [1; 2] [3] q,
        { },
        Var counter: 9
      >

      No answer

    ----------------------------------------------

    Step 1:
      <
        appendo [1; 2] [3] _.10,
        { },
        Var counter: 10
      >

      No answer

    ----------------------------------------------

    Step 2:
      mplus
        <
          [1; 2] === [] &&& [3] === _.10,
          { },
          Var counter: 10
        >
        Thunk
          <
            fresh (h t ab') [1; 2] === [h; t] &&& [h; ab'] === _.10 &&& appendo t [3] ab',
            { },
            Var counter: 10
          >

      No answer

    ----------------------------------------------

    Step 3:
      <
        [1; 2] === [_.11; _.12] &&& [_.11; _.13] === _.10 &&& appendo _.12 [3] _.13,
        { },
        Var counter: 13
      >

      No answer

    ----------------------------------------------

    Step 4:
      mplus
        force
          Thunk
            bind
              force
                Thunk
                  mplus
                    force
                      Thunk
                        bind
                          force
                            Nil
                          [_.11; _.13] === _.10
                    Nil
              appendo _.12 [3] _.13
        Thunk
          mplus
            <
              _.12 === [] &&& [3] === _.13,
              {
                _.10 <- [_.11; _.13];
                _.11 <- 1;
                _.12 <- [2]
              },
              Var counter: 13
            >
            Thunk
              <
                fresh (h t ab') _.12 === [h; t] &&& [h; ab'] === _.13 &&& appendo t [3] ab',
                {
                  _.10 <- [_.11; _.13];
                  _.11 <- 1;
                  _.12 <- [2]
                },
                Var counter: 13
              >

      No answer

    ----------------------------------------------

    Step 5:
      <
        _.12 === [_.14; _.15] &&& [_.14; _.16] === _.13 &&& appendo _.15 [3] _.16,
        {
          _.10 <- [_.11; _.13];
          _.11 <- 1;
          _.12 <- [2]
        },
        Var counter: 16
      >

      No answer

    ----------------------------------------------

    Step 6:
      mplus
        force
          Thunk
            bind
              force
                Thunk
                  mplus
                    force
                      Thunk
                        bind
                          force
                            Nil
                          [_.14; _.16] === _.13
                    Nil
              appendo _.15 [3] _.16
        Thunk
          mplus
            <
              _.15 === [] &&& [3] === _.16,
              {
                _.10 <- [_.11; _.13];
                _.11 <- 1;
                _.12 <- [2];
                _.13 <- [_.14; _.16];
                _.14 <- 2;
                _.15 <- []
              },
              Var counter: 16
            >
            Thunk
              <
                fresh (h t ab') _.15 === [h; t] &&& [h; ab'] === _.16 &&& appendo t [3] ab',
                {
                  _.10 <- [_.11; _.13];
                  _.11 <- 1;
                  _.12 <- [2];
                  _.13 <- [_.14; _.16];
                  _.14 <- 2;
                  _.15 <- []
                },
                Var counter: 16
              >

      Answer:
        {
          _.10 <- [_.11; _.13];
          _.11 <- 1;
          _.12 <- [2];
          _.13 <- [_.14; _.16];
          _.14 <- 2;
          _.15 <- [];
          _.16 <- [3]
        },
        Var counter: 16

    ----------------------------------------------

    Step 7:
      Thunk
        mplus
          force
            Thunk
              <
                fresh (h t ab') _.15 === [h; t] &&& [h; ab'] === _.16 &&& appendo t [3] ab',
                {
                  _.10 <- [_.11; _.13];
                  _.11 <- 1;
                  _.12 <- [2];
                  _.13 <- [_.14; _.16];
                  _.14 <- 2;
                  _.15 <- []
                },
                Var counter: 16
              >
          Thunk
            mplus
              force
                Thunk
                  bind
                    force
                      Nil
                    [3] === _.16
              Nil

      No answer

    ----------------------------------------------

    Step 8:
      mplus
        force
          Thunk
            <
              fresh (h t ab') _.15 === [h; t] &&& [h; ab'] === _.16 &&& appendo t [3] ab',
              {
                _.10 <- [_.11; _.13];
                _.11 <- 1;
                _.12 <- [2];
                _.13 <- [_.14; _.16];
                _.14 <- 2;
                _.15 <- []
              },
              Var counter: 16
            >
        Thunk
          mplus
            force
              Thunk
                bind
                  force
                    Nil
                  [3] === _.16
            Nil

      No answer

    ----------------------------------------------

    Step 9:
      mplus
        force
          Thunk
            mplus
              force
                Thunk
                  bind
                    force
                      Nil
                    [3] === _.16
              Nil
        Thunk
          <
            _.15 === [_.17; _.18] &&& [_.17; _.19] === _.16 &&& appendo _.18 [3] _.19,
            {
              _.10 <- [_.11; _.13];
              _.11 <- 1;
              _.12 <- [2];
              _.13 <- [_.14; _.16];
              _.14 <- 2;
              _.15 <- []
            },
            Var counter: 19
          >

      No answer |}]
