open Stream

let%expect_test "" =
  let open Examples in
  print_trace Ocanren_trace.eval [ appendo ] (-1)
    (fresh [ "q" ] @@ invoke "appendo" [ list1; list2; Var "q" ]);
  [%expect
    {|
    ----------------------------------------------

    Step 0:
      No answer,
      <
        fresh (q) appendo Cons(1, Cons(2, Nil)) Cons(3, Nil) q,
        { },
        Var counter: 0
      >

    ----------------------------------------------

    Step 1:
      No answer,
      <
        appendo Cons(1, Cons(2, Nil)) Cons(3, Nil) _.1,
        { },
        Var counter: 1
      >

    ----------------------------------------------

    Step 2:
      No answer,
      mplus
        <
          Cons(1, Cons(2, Nil)) === Nil &&& _.1 === Cons(3, Nil),
          { },
          Var counter: 1
        >
        <
          fresh (e xs xys) Cons(1, Cons(2, Nil)) === Cons(e, xs) &&& _.1 === Cons(e, xys) &&& appendo xs Cons(3, Nil) xys,
          { },
          Var counter: 1
        >

    ----------------------------------------------

    Step 3:
      No answer,
      <
        Cons(1, Cons(2, Nil)) === Cons(_.2, _.3) &&& _.1 === Cons(_.2, _.4) &&& appendo _.3 Cons(3, Nil) _.4,
        { },
        Var counter: 4
      >

    ----------------------------------------------

    Step 4:
      No answer,
      mplus
        bind
          mplus
            bind
              nil
              _.1 === Cons(_.2, _.4)
            nil
          appendo _.3 Cons(3, Nil) _.4
        mplus
          <
            _.3 === Nil &&& _.4 === Cons(3, Nil),
            {
              _.1 <- Cons(_.2, _.4);
              _.2 <- 1;
              _.3 <- Cons(2, Nil)
            },
            Var counter: 4
          >
          <
            fresh (e xs xys) _.3 === Cons(e, xs) &&& _.4 === Cons(e, xys) &&& appendo xs Cons(3, Nil) xys,
            {
              _.1 <- Cons(_.2, _.4);
              _.2 <- 1;
              _.3 <- Cons(2, Nil)
            },
            Var counter: 4
          >

    ----------------------------------------------

    Step 5:
      No answer,
      <
        _.3 === Cons(_.5, _.6) &&& _.4 === Cons(_.5, _.7) &&& appendo _.6 Cons(3, Nil) _.7,
        {
          _.1 <- Cons(_.2, _.4);
          _.2 <- 1;
          _.3 <- Cons(2, Nil)
        },
        Var counter: 7
      >

    ----------------------------------------------

    Step 6:
      Answer:
        {
          _.1 <- Cons(_.2, _.4);
          _.2 <- 1;
          _.3 <- Cons(2, Nil);
          _.4 <- Cons(_.5, _.7);
          _.5 <- 2;
          _.6 <- Nil;
          _.7 <- Cons(3, Nil)
        },
        Var counter: 7,
      mplus
        bind
          mplus
            bind
              nil
              _.4 === Cons(_.5, _.7)
            nil
          appendo _.6 Cons(3, Nil) _.7
        mplus
          <
            _.6 === Nil &&& _.7 === Cons(3, Nil),
            {
              _.1 <- Cons(_.2, _.4);
              _.2 <- 1;
              _.3 <- Cons(2, Nil);
              _.4 <- Cons(_.5, _.7);
              _.5 <- 2;
              _.6 <- Nil
            },
            Var counter: 7
          >
          <
            fresh (e xs xys) _.6 === Cons(e, xs) &&& _.7 === Cons(e, xys) &&& appendo xs Cons(3, Nil) xys,
            {
              _.1 <- Cons(_.2, _.4);
              _.2 <- 1;
              _.3 <- Cons(2, Nil);
              _.4 <- Cons(_.5, _.7);
              _.5 <- 2;
              _.6 <- Nil
            },
            Var counter: 7
          >

    ----------------------------------------------

    Step 7:
      No answer,
      mplus
        <
          fresh (e xs xys) _.6 === Cons(e, xs) &&& _.7 === Cons(e, xys) &&& appendo xs Cons(3, Nil) xys,
          {
            _.1 <- Cons(_.2, _.4);
            _.2 <- 1;
            _.3 <- Cons(2, Nil);
            _.4 <- Cons(_.5, _.7);
            _.5 <- 2;
            _.6 <- Nil
          },
          Var counter: 7
        >
        mplus
          bind
            nil
            _.7 === Cons(3, Nil)
          nil

    ----------------------------------------------

    Step 8:
      No answer,
      mplus
        mplus
          bind
            nil
            _.7 === Cons(3, Nil)
          nil
        <
          _.6 === Cons(_.8, _.9) &&& _.7 === Cons(_.8, _.10) &&& appendo _.9 Cons(3, Nil) _.10,
          {
            _.1 <- Cons(_.2, _.4);
            _.2 <- 1;
            _.3 <- Cons(2, Nil);
            _.4 <- Cons(_.5, _.7);
            _.5 <- 2;
            _.6 <- Nil
          },
          Var counter: 10
        >

    ----------------------------------------------

    Step 9:
      No answer,
      nil |}]

let%expect_test "" =
  let open Examples in
  print_trace Ocanren_trace.eval [ appendo; reverso ] (-1)
    (fresh [ "q" ] @@ invoke "reverso" [ list1; Var "q" ]);
  [%expect
    {|
    ----------------------------------------------

    Step 0:
      No answer,
      <
        fresh (q) reverso Cons(1, Cons(2, Nil)) q,
        { },
        Var counter: 0
      >

    ----------------------------------------------

    Step 1:
      No answer,
      <
        reverso Cons(1, Cons(2, Nil)) _.1,
        { },
        Var counter: 1
      >

    ----------------------------------------------

    Step 2:
      No answer,
      mplus
        <
          Cons(1, Cons(2, Nil)) === Nil &&& _.1 === Nil,
          { },
          Var counter: 1
        >
        <
          fresh (e xs ys) Cons(1, Cons(2, Nil)) === Cons(e, xs) &&& reverso xs ys &&& appendo ys Cons(e, Nil) _.1,
          { },
          Var counter: 1
        >

    ----------------------------------------------

    Step 3:
      No answer,
      <
        Cons(1, Cons(2, Nil)) === Cons(_.2, _.3) &&& reverso _.3 _.4 &&& appendo _.4 Cons(_.2, Nil) _.1,
        { },
        Var counter: 4
      >

    ----------------------------------------------

    Step 4:
      No answer,
      bind
        mplus
          bind
            nil
            reverso _.3 _.4
          mplus
            <
              _.3 === Nil &&& _.4 === Nil,
              {
                _.2 <- 1;
                _.3 <- Cons(2, Nil)
              },
              Var counter: 4
            >
            <
              fresh (e xs ys) _.3 === Cons(e, xs) &&& reverso xs ys &&& appendo ys Cons(e, Nil) _.4,
              {
                _.2 <- 1;
                _.3 <- Cons(2, Nil)
              },
              Var counter: 4
            >
        appendo _.4 Cons(_.2, Nil) _.1

    ----------------------------------------------

    Step 5:
      No answer,
      bind
        <
          _.3 === Cons(_.5, _.6) &&& reverso _.6 _.7 &&& appendo _.7 Cons(_.5, Nil) _.4,
          {
            _.2 <- 1;
            _.3 <- Cons(2, Nil)
          },
          Var counter: 7
        >
        appendo _.4 Cons(_.2, Nil) _.1

    ----------------------------------------------

    Step 6:
      No answer,
      bind
        bind
          mplus
            bind
              nil
              reverso _.6 _.7
            mplus
              <
                _.6 === Nil &&& _.7 === Nil,
                {
                  _.2 <- 1;
                  _.3 <- Cons(2, Nil);
                  _.5 <- 2;
                  _.6 <- Nil
                },
                Var counter: 7
              >
              <
                fresh (e xs ys) _.6 === Cons(e, xs) &&& reverso xs ys &&& appendo ys Cons(e, Nil) _.7,
                {
                  _.2 <- 1;
                  _.3 <- Cons(2, Nil);
                  _.5 <- 2;
                  _.6 <- Nil
                },
                Var counter: 7
              >
          appendo _.7 Cons(_.5, Nil) _.4
        appendo _.4 Cons(_.2, Nil) _.1

    ----------------------------------------------

    Step 7:
      No answer,
      bind
        mplus
          bind
            mplus
              <
                fresh (e xs ys) _.6 === Cons(e, xs) &&& reverso xs ys &&& appendo ys Cons(e, Nil) _.7,
                {
                  _.2 <- 1;
                  _.3 <- Cons(2, Nil);
                  _.5 <- 2;
                  _.6 <- Nil
                },
                Var counter: 7
              >
              mplus
                bind
                  nil
                  _.7 === Nil
                nil
            appendo _.7 Cons(_.5, Nil) _.4
          mplus
            <
              _.7 === Nil &&& _.4 === Cons(_.5, Nil),
              {
                _.2 <- 1;
                _.3 <- Cons(2, Nil);
                _.5 <- 2;
                _.6 <- Nil;
                _.7 <- Nil
              },
              Var counter: 7
            >
            <
              fresh (e xs xys) _.7 === Cons(e, xs) &&& _.4 === Cons(e, xys) &&& appendo xs Cons(_.5, Nil) xys,
              {
                _.2 <- 1;
                _.3 <- Cons(2, Nil);
                _.5 <- 2;
                _.6 <- Nil;
                _.7 <- Nil
              },
              Var counter: 7
            >
        appendo _.4 Cons(_.2, Nil) _.1

    ----------------------------------------------

    Step 8:
      No answer,
      bind
        mplus
          mplus
            <
              _.7 === Nil &&& _.4 === Cons(_.5, Nil),
              {
                _.2 <- 1;
                _.3 <- Cons(2, Nil);
                _.5 <- 2;
                _.6 <- Nil;
                _.7 <- Nil
              },
              Var counter: 7
            >
            <
              fresh (e xs xys) _.7 === Cons(e, xs) &&& _.4 === Cons(e, xys) &&& appendo xs Cons(_.5, Nil) xys,
              {
                _.2 <- 1;
                _.3 <- Cons(2, Nil);
                _.5 <- 2;
                _.6 <- Nil;
                _.7 <- Nil
              },
              Var counter: 7
            >
          bind
            mplus
              mplus
                bind
                  nil
                  _.7 === Nil
                nil
              <
                _.6 === Cons(_.8, _.9) &&& reverso _.9 _.10 &&& appendo _.10 Cons(_.8, Nil) _.7,
                {
                  _.2 <- 1;
                  _.3 <- Cons(2, Nil);
                  _.5 <- 2;
                  _.6 <- Nil
                },
                Var counter: 10
              >
            appendo _.7 Cons(_.5, Nil) _.4
        appendo _.4 Cons(_.2, Nil) _.1

    ----------------------------------------------

    Step 9:
      No answer,
      mplus
        bind
          mplus
            bind
              mplus
                mplus
                  bind
                    nil
                    _.7 === Nil
                  nil
                <
                  _.6 === Cons(_.8, _.9) &&& reverso _.9 _.10 &&& appendo _.10 Cons(_.8, Nil) _.7,
                  {
                    _.2 <- 1;
                    _.3 <- Cons(2, Nil);
                    _.5 <- 2;
                    _.6 <- Nil
                  },
                  Var counter: 10
                >
              appendo _.7 Cons(_.5, Nil) _.4
            mplus
              <
                fresh (e xs xys) _.7 === Cons(e, xs) &&& _.4 === Cons(e, xys) &&& appendo xs Cons(_.5, Nil) xys,
                {
                  _.2 <- 1;
                  _.3 <- Cons(2, Nil);
                  _.5 <- 2;
                  _.6 <- Nil;
                  _.7 <- Nil
                },
                Var counter: 7
              >
              mplus
                bind
                  nil
                  _.4 === Cons(_.5, Nil)
                nil
          appendo _.4 Cons(_.2, Nil) _.1
        mplus
          <
            _.4 === Nil &&& _.1 === Cons(_.2, Nil),
            {
              _.2 <- 1;
              _.3 <- Cons(2, Nil);
              _.4 <- Cons(_.5, Nil);
              _.5 <- 2;
              _.6 <- Nil;
              _.7 <- Nil
            },
            Var counter: 7
          >
          <
            fresh (e xs xys) _.4 === Cons(e, xs) &&& _.1 === Cons(e, xys) &&& appendo xs Cons(_.2, Nil) xys,
            {
              _.2 <- 1;
              _.3 <- Cons(2, Nil);
              _.4 <- Cons(_.5, Nil);
              _.5 <- 2;
              _.6 <- Nil;
              _.7 <- Nil
            },
            Var counter: 7
          >

    ----------------------------------------------

    Step 10:
      No answer,
      mplus
        mplus
          <
            _.4 === Nil &&& _.1 === Cons(_.2, Nil),
            {
              _.2 <- 1;
              _.3 <- Cons(2, Nil);
              _.4 <- Cons(_.5, Nil);
              _.5 <- 2;
              _.6 <- Nil;
              _.7 <- Nil
            },
            Var counter: 7
          >
          <
            fresh (e xs xys) _.4 === Cons(e, xs) &&& _.1 === Cons(e, xys) &&& appendo xs Cons(_.2, Nil) xys,
            {
              _.2 <- 1;
              _.3 <- Cons(2, Nil);
              _.4 <- Cons(_.5, Nil);
              _.5 <- 2;
              _.6 <- Nil;
              _.7 <- Nil
            },
            Var counter: 7
          >
        bind
          mplus
            mplus
              bind
                nil
                _.4 === Cons(_.5, Nil)
              nil
            <
              _.7 === Cons(_.8, _.9) &&& _.4 === Cons(_.8, _.10) &&& appendo _.9 Cons(_.5, Nil) _.10,
              {
                _.2 <- 1;
                _.3 <- Cons(2, Nil);
                _.5 <- 2;
                _.6 <- Nil;
                _.7 <- Nil
              },
              Var counter: 10
            >
          appendo _.4 Cons(_.2, Nil) _.1

    ----------------------------------------------

    Step 11:
      No answer,
      mplus
        bind
          mplus
            mplus
              bind
                nil
                _.4 === Cons(_.5, Nil)
              nil
            <
              _.7 === Cons(_.8, _.9) &&& _.4 === Cons(_.8, _.10) &&& appendo _.9 Cons(_.5, Nil) _.10,
              {
                _.2 <- 1;
                _.3 <- Cons(2, Nil);
                _.5 <- 2;
                _.6 <- Nil;
                _.7 <- Nil
              },
              Var counter: 10
            >
          appendo _.4 Cons(_.2, Nil) _.1
        <
          _.4 === Cons(_.8, _.9) &&& _.1 === Cons(_.8, _.10) &&& appendo _.9 Cons(_.2, Nil) _.10,
          {
            _.2 <- 1;
            _.3 <- Cons(2, Nil);
            _.4 <- Cons(_.5, Nil);
            _.5 <- 2;
            _.6 <- Nil;
            _.7 <- Nil
          },
          Var counter: 10
        >

    ----------------------------------------------

    Step 12:
      Answer:
        {
          _.1 <- Cons(_.8, _.10);
          _.2 <- 1;
          _.3 <- Cons(2, Nil);
          _.4 <- Cons(_.5, Nil);
          _.5 <- 2;
          _.6 <- Nil;
          _.7 <- Nil;
          _.8 <- 2;
          _.9 <- Nil;
          _.10 <- Cons(_.2, Nil)
        },
        Var counter: 10,
      mplus
        bind
          mplus
            bind
              nil
              _.1 === Cons(_.8, _.10)
            nil
          appendo _.9 Cons(_.2, Nil) _.10
        mplus
          <
            _.9 === Nil &&& _.10 === Cons(_.2, Nil),
            {
              _.1 <- Cons(_.8, _.10);
              _.2 <- 1;
              _.3 <- Cons(2, Nil);
              _.4 <- Cons(_.5, Nil);
              _.5 <- 2;
              _.6 <- Nil;
              _.7 <- Nil;
              _.8 <- 2;
              _.9 <- Nil
            },
            Var counter: 10
          >
          <
            fresh (e xs xys) _.9 === Cons(e, xs) &&& _.10 === Cons(e, xys) &&& appendo xs Cons(_.2, Nil) xys,
            {
              _.1 <- Cons(_.8, _.10);
              _.2 <- 1;
              _.3 <- Cons(2, Nil);
              _.4 <- Cons(_.5, Nil);
              _.5 <- 2;
              _.6 <- Nil;
              _.7 <- Nil;
              _.8 <- 2;
              _.9 <- Nil
            },
            Var counter: 10
          >

    ----------------------------------------------

    Step 13:
      No answer,
      mplus
        <
          fresh (e xs xys) _.9 === Cons(e, xs) &&& _.10 === Cons(e, xys) &&& appendo xs Cons(_.2, Nil) xys,
          {
            _.1 <- Cons(_.8, _.10);
            _.2 <- 1;
            _.3 <- Cons(2, Nil);
            _.4 <- Cons(_.5, Nil);
            _.5 <- 2;
            _.6 <- Nil;
            _.7 <- Nil;
            _.8 <- 2;
            _.9 <- Nil
          },
          Var counter: 10
        >
        mplus
          bind
            nil
            _.10 === Cons(_.2, Nil)
          nil

    ----------------------------------------------

    Step 14:
      No answer,
      mplus
        mplus
          bind
            nil
            _.10 === Cons(_.2, Nil)
          nil
        <
          _.9 === Cons(_.11, _.12) &&& _.10 === Cons(_.11, _.13) &&& appendo _.12 Cons(_.2, Nil) _.13,
          {
            _.1 <- Cons(_.8, _.10);
            _.2 <- 1;
            _.3 <- Cons(2, Nil);
            _.4 <- Cons(_.5, Nil);
            _.5 <- 2;
            _.6 <- Nil;
            _.7 <- Nil;
            _.8 <- 2;
            _.9 <- Nil
          },
          Var counter: 13
        >

    ----------------------------------------------

    Step 15:
      No answer,
      nil |}]
