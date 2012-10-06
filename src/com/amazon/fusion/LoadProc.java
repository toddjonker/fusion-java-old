// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class LoadProc
    extends Procedure1
{
    private final LoadHandler myLoadHandler;

    LoadProc(LoadHandler loadHandler)
    {
        //    "                                                                               |
        super("Opens the Fusion source file named by the given string and evaluates each\n" +
              "expression in sequence. Returns the last result.\n" +
              "FILENAME is resolved relative to the value of current_directory.",
              "filename");

        myLoadHandler = loadHandler;
    }


    @Override
    public Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        String fileName = checkStringArg(0, arg);

        return myLoadHandler.loadTopLevel(eval, null, fileName);
    }
}
