// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxNull
    extends SyntaxValue
{
    private SyntaxNull(SourceLocation loc)
    {
        super(loc);
    }

    static SyntaxNull make(SourceLocation loc)
    {
        return new SyntaxNull(loc);
    }


    @Override
    Type getType()
    {
        return Type.NULL;
    }

    @Override
    FusionValue eval(Evaluator eval, Environment env) throws FusionException
    {
        return eval.newNull();
    }

    @Override
    FusionValue quote(Evaluator eval)
    {
        return eval.newNull();
    }

    @Override
    void writeTo(IonWriter writer) throws IOException
    {
        writer.writeNull();
    }

    @Override
    boolean isNullValue()
    {
        return true;
    }
}
