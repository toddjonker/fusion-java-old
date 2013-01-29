// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * "Registers" used during macro expansion
 */
final class ExpandContext
{
    enum Context { TOP, MODULE, EXPRESSION }

    final Evaluator myEval;
    final Context   myContext;

    ExpandContext(Evaluator eval)
    {
        myEval = eval;
        myContext = Context.TOP;
    }

    private ExpandContext(Evaluator eval, Context ctx)
    {
        myEval = eval;
        myContext = ctx;
    }

    boolean isTopLevel()
    {
        return myContext == Context.TOP;
    }

    ExpandContext nestModule()
    {
        assert isTopLevel();

        return new ExpandContext(myEval, Context.MODULE);
    }
}
