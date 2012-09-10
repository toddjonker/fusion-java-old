// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonWriter;
import com.amazon.ion.Timestamp;
import java.io.IOException;

final class SyntaxTimestamp
    extends SyntaxValue
{
    private final Timestamp myValue;


    private SyntaxTimestamp(Timestamp value, String[] anns, SourceLocation loc)
    {
        super(anns, loc);
        myValue = value;
    }

    static SyntaxTimestamp make(Timestamp value, String[] anns,
                                SourceLocation loc)
    {
        return new SyntaxTimestamp(value, anns, loc);
    }


    @Override
    Type getType()
    {
        return Type.TIMESTAMP;
    }


    @Override
    FusionValue doCompileIonConstant(Evaluator eval, Environment env)
        throws FusionException
    {
        return eval.newTimestamp(myValue);
    }


    @Override
    FusionValue quote(Evaluator eval)
    {
        return eval.newTimestamp(myValue, getAnnotations());
    }


    @Override
    void writeContentTo(IonWriter writer) throws IOException
    {
        writer.writeTimestamp(myValue);
    }


    @Override
    boolean isNullValue()
    {
        return myValue == null;
    }
}
