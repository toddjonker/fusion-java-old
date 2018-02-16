// Copyright (c) 2012-2018 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionString.checkNonEmptyStringArg;


final class LoadProc
    extends Procedure1
{
    private final LoadHandler myLoadHandler;

    LoadProc(LoadHandler loadHandler)
    {
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
