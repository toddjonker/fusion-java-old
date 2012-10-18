// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVoid.voidValue;


final class CompiledVoid
    implements CompiledForm
{
    private CompiledVoid() {}

    static final CompiledVoid SINGLETON = new CompiledVoid();

    @Override
    public Object doEval(Evaluator eval, Store store)
        throws FusionException
    {
        return voidValue(eval);
    }
}
