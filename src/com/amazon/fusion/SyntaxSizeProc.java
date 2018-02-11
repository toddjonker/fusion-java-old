// Copyright (c) 2012-2018 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.makeInt;

final class SyntaxSizeProc
    extends Procedure1
{
    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        SyntaxSequence c = checkSyntaxSequenceArg(0, arg);
        return makeInt(eval, c.size());
    }
}
