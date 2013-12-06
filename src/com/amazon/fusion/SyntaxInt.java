// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.makeInt;
import com.amazon.fusion.FusionNumber.BaseInt;
import com.amazon.ion.IonException;
import com.amazon.ion.IonWriter;
import java.io.IOException;
import java.math.BigInteger;

final class SyntaxInt
    extends SyntaxValue
{
    private final BaseInt myDatum;


    /**
     * @param datum must not be null.
     */
    private SyntaxInt(SourceLocation loc, BaseInt datum)
    {
        super(datum.annotationsAsJavaStrings(), loc);
        myDatum = datum;
    }


    /**
     * @param datum must be a Fusion int.
     */
    static SyntaxInt make(Evaluator eval, SourceLocation loc, Object datum)
    {
        return new SyntaxInt(loc, (BaseInt) datum);
    }


    static SyntaxInt make(Evaluator      eval,
                          SourceLocation loc,
                          String[]       annotations,
                          BigInteger     value)
    {
        BaseInt datum = makeInt(eval, annotations, value);
        return new SyntaxInt(loc, datum);
    }


    static SyntaxInt make(Evaluator eval, int value)
    {
        BaseInt datum = makeInt(eval, value);
        return new SyntaxInt(/*location*/ null, datum);
    }


    //========================================================================


    @Override
    Type getType()
    {
        return Type.INT;
    }


    @Override
    CompiledForm doCompile(Evaluator eval, Environment env)
        throws FusionException
    {
        return new CompiledConstant(myDatum);
    }


    @Override
    Object unwrap(Evaluator eval, boolean recurse)
    {
        return myDatum;
    }


    @Override
    void ionize(Evaluator eval, IonWriter writer)
        throws IOException, IonException, FusionException, IonizeFailure
    {
        myDatum.ionize(eval, writer);
    }


    @Override
    boolean isNullValue()
    {
        return myDatum.isAnyNull();
    }
}
