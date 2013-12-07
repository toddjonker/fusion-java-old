// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.makeFloat;
import com.amazon.fusion.FusionNumber.BaseFloat;
import com.amazon.ion.IonException;
import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxFloat
    extends SyntaxValue
{
    private final BaseFloat myDatum;


    /**
     * @param datum must not be null.
     */
    private SyntaxFloat(SourceLocation loc, BaseFloat datum)
    {
        super(datum.annotationsAsJavaStrings(), loc);
        myDatum = datum;
    }


    /**
     * @param datum must be a Fusion decimal.
     */
    static SyntaxFloat make(Evaluator eval, SourceLocation loc, Object datum)
    {
        return new SyntaxFloat(loc, (BaseFloat) datum);
    }

    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     */
    static SyntaxFloat make(Evaluator      eval,
                            SourceLocation loc,
                            String[]       annotations,
                            double         value)
    {
        BaseFloat datum = makeFloat(eval, annotations, value);
        return new SyntaxFloat(loc, datum);
    }

    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     * @param value may be null.
     */
    static SyntaxFloat make(Evaluator      eval,
                            SourceLocation loc,
                            String[]       annotations,
                            Double         value)
    {
        BaseFloat datum = makeFloat(eval, annotations, value);
        return new SyntaxFloat(loc, datum);
    }


    //========================================================================


    @Override
    Type getType()
    {
        return Type.FLOAT;
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
