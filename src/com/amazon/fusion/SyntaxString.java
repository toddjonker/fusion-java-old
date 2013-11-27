// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionString.makeString;
import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import com.amazon.fusion.FusionString.BaseString;
import com.amazon.ion.IonException;
import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxString
    extends SyntaxText
{
    private final BaseString myDatum;

    /**
     * @param datum must not be null.
     */
    private SyntaxString(Evaluator eval, SourceLocation loc, BaseString datum)
    {
        super(datum.stringValue(), datum.annotationsAsJavaStrings(), loc);
        myDatum = datum;
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     * @param value may be null.
     */
    private SyntaxString(Evaluator eval,
                         SourceLocation loc,
                         String[] annotations,
                         String value)
    {
        super(value, annotations, loc);
        myDatum = makeString(eval, annotations, value);
    }


    /**
     * @param datum must not be null.
     */
    static SyntaxString make(Evaluator eval,
                             SourceLocation loc,
                             BaseString datum)
    {
        return new SyntaxString(eval, loc, datum);
    }


    /**
     * @param datum must be a Fusion string.
     */
    static SyntaxString make(Evaluator eval, SourceLocation loc, Object datum)
    {
        BaseString string = (BaseString) datum;
        return new SyntaxString(eval, loc, string);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     * @param value may be null.
     */
    static SyntaxString make(Evaluator eval,
                             SourceLocation loc,
                             String[] annotations,
                             String value)
    {
        return new SyntaxString(eval, loc, annotations, value);
    }


    /**
     * @param value may be null.
     */
    static SyntaxString make(Evaluator eval, String value)
    {
        return new SyntaxString(eval, null, EMPTY_STRING_ARRAY, value);
    }


    //========================================================================


    @Override
    Type getType()
    {
        return Type.STRING;
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
}
