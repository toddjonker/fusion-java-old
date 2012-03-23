// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;

final class FuncKeyword
    extends KeywordValue
{
    FuncKeyword(String keyword)
    {
        super(keyword);
    }

    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
    {
        return new FuncValue(expr, env);
    }
}
