// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxKeyword
    extends SyntaxText
{
    private SyntaxKeyword(String value, String[] anns, SourceLocation loc)
    {
        super(value, anns, loc);
    }

    static SyntaxKeyword make(String value)
    {
        return new SyntaxKeyword(value, EMPTY_STRING_ARRAY, null);
    }

    static SyntaxKeyword make(String value, String[] anns, SourceLocation loc)
    {
        return new SyntaxKeyword(value, anns, loc);
    }


    @Override
    Type getType()
    {
        return Type.KEYWORD;
    }


    @Override
    SyntaxValue doExpand(Expander eval, Environment env)
        throws SyntaxFailure
    {
        throw new SyntaxFailure(null, "Keywords are not expressions", this);
    }


    @Override
    Object unwrap(Evaluator eval, boolean recurse)
    {
        // TODO I have no idea if this is correct long-term.
        // Should we allow it at the moment?
        return eval.newSymbol(myText, getAnnotations());
    }


    @Override
    void ionize(Evaluator eval, IonWriter writer)
        throws IOException
    {
        ionizeAnnotations(writer);
        writer.writeSymbol(myText);  // TODO __ ??
    }


    //========================================================================


    @Override
    CompiledForm doCompile(Evaluator eval, Environment env)
        throws FusionException
    {
        throw new IllegalStateException("Should not get here");
    }
}
