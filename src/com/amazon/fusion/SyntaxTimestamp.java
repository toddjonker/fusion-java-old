// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionTimestamp.makeTimestamp;
import com.amazon.fusion.FusionTimestamp.BaseTimestamp;
import com.amazon.ion.IonException;
import com.amazon.ion.IonWriter;
import com.amazon.ion.Timestamp;
import java.io.IOException;

final class SyntaxTimestamp
    extends SyntaxValue
{
    private final BaseTimestamp myDatum;


    private SyntaxTimestamp(SourceLocation loc, BaseTimestamp datum)
    {
        super(datum.annotationsAsJavaStrings(), loc);
        myDatum = datum;
    }


    /**
     * @param fusionTimestamp must be a Fusion timestamp.
     */
    static SyntaxTimestamp make(Evaluator      eval,
                                SourceLocation loc,
                                Object         fusionTimestamp)
    {
        BaseTimestamp ts = (BaseTimestamp) fusionTimestamp;
        return new SyntaxTimestamp(loc, ts);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     * @param value may be null.
     */
    static SyntaxTimestamp make(Evaluator      eval,
                                SourceLocation loc,
                                String[]       annotations,
                                Timestamp      value)
    {
        BaseTimestamp ts = makeTimestamp(eval, annotations, value);
        return new SyntaxTimestamp(loc, ts);
    }


    //========================================================================


    @Override
    Type getType()
    {
        return Type.TIMESTAMP;
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
