// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionString.checkNonEmptyStringArg;


final class LoadProc
    extends Procedure1
{
    private final LoadHandler myLoadHandler;

    LoadProc(LoadHandler loadHandler)
    {
        //    "                                                                               |
        super("Opens the Fusion source file named by the given string and evaluates each\n" +
              "expression in sequence, returning the last result.  The `filename` is resolved\n" +
              "relative to the value of the [`current_directory`](fusion/io.html#current_directory)\n" +
              "parameter.  The evaluation is performed within the [current namespace](fusion/namespace.html#current_namespace).",
              "filename");

        myLoadHandler = loadHandler;
    }


    @Override
    public Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        String fileName = checkNonEmptyStringArg(eval, this, 0, arg);

        return myLoadHandler.loadTopLevel(eval, null, fileName);
    }
}
