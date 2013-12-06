// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.makeDecimal;
import com.amazon.fusion.FusionNumber.BaseDecimal;
import com.amazon.ion.IonException;
import com.amazon.ion.IonWriter;
import java.io.IOException;
import java.math.BigDecimal;

final class SyntaxDecimal
    extends SyntaxValue
{
    private final BaseDecimal myDatum;


    /**
     * @param datum must not be null.
     */
    private SyntaxDecimal(SourceLocation loc, BaseDecimal datum)
    {
        super(datum.annotationsAsJavaStrings(), loc);
        myDatum = datum;
    }


    /**
     * @param datum must be a Fusion decimal.
     */
    static SyntaxDecimal make(Evaluator eval, SourceLocation loc, Object datum)
    {
        return new SyntaxDecimal(loc, (BaseDecimal) datum);
    }

    static SyntaxDecimal make(Evaluator      eval,
                              SourceLocation loc,
                              String[]       annotations,
                              BigDecimal     value)
    {
        BaseDecimal datum = makeDecimal(eval, annotations, value);
        return new SyntaxDecimal(loc, datum);
    }


    //========================================================================


    @Override
    Type getType()
    {
        return Type.DECIMAL;
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
