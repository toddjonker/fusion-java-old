// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionSymbol.makeSymbol;
import com.amazon.fusion.FusionSymbol.BaseSymbol;
import com.amazon.ion.IonException;
import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxKeyword
    extends SyntaxText
{
    private final BaseSymbol myDatum;

    /**
     * @param datum must not be null.
     */
    private SyntaxKeyword(Evaluator eval, SourceLocation loc, BaseSymbol datum)
    {
        super(datum.stringValue(), datum.annotationsAsJavaStrings(), loc);
        myDatum = datum;
    }


    private SyntaxKeyword(Evaluator eval,
                          SourceLocation loc,
                          String[] annotations,
                          String value)
    {
        super(value, annotations, loc);
        myDatum = makeSymbol(eval, annotations, value);
    }


    /**
     * @param datum must be a Fusion symbol.
     */
    static SyntaxKeyword make(Evaluator eval, SourceLocation loc, Object datum)
    {
        BaseSymbol symbol = (BaseSymbol) datum;
        return new SyntaxKeyword(eval, loc, symbol);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     */
    static SyntaxKeyword make(Evaluator eval,
                              SourceLocation loc,
                              String[] annotations,
                              String value)
    {
        return new SyntaxKeyword(eval, loc, annotations, value);
    }


    //========================================================================


    @Override
    Type getType()
    {
        return Type.KEYWORD;
    }


    @Override
    SyntaxValue doExpand(Expander eval, Environment env)
        throws SyntaxException
    {
        throw new SyntaxException(null, "Keywords are not expressions", this);
    }


    @Override
    Object unwrap(Evaluator eval, boolean recurse)
    {
        // TODO I have no idea if this is correct long-term.
        // Should we allow it at the moment?
        return myDatum;
    }


    @Override
    void ionize(Evaluator eval, IonWriter writer)
        throws IOException, IonException, FusionException, IonizeFailure
    {
        // TODO __ ??
        myDatum.ionize(eval, writer);
    }


    //========================================================================


    @Override
    CompiledForm doCompile(Evaluator eval, Environment env)
        throws FusionException
    {
        throw new IllegalStateException("Should not get here");
    }
}
