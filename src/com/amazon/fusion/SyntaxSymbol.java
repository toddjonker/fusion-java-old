// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import com.amazon.fusion.Environment.Binding;
import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxSymbol
    extends SyntaxText
{
    /** Initialized during {@link #prepare} */
    private Binding myBinding;


    private SyntaxSymbol(String value, String[] anns, SourceLocation loc)
    {
        super(value, anns, loc);
    }

    static SyntaxSymbol make(String value)
    {
        return new SyntaxSymbol(value, EMPTY_STRING_ARRAY, null);
    }

    static SyntaxSymbol make(String value, String[] anns, SourceLocation loc)
    {
        return new SyntaxSymbol(value, anns, loc);
    }


    @Override
    Type getType()
    {
        return Type.SYMBOL;
    }


    /** Not set until {@link #prepare}. */
    Binding getBinding()
    {
        return myBinding;
    }

    /** Expand-time binding resolution. */
    Environment.Binding resolve()
    {
        if (myWraps == null) return null;

        for (SyntaxWrap w : myWraps)
        {
            Environment.Binding resolution = w.resolve(myText);
            if (resolution != null) return resolution;
        }
        return null;
    }

    @Override
    SyntaxValue prepare(Evaluator eval, Environment env)
        throws SyntaxFailure
    {
        if (myBinding == null)  // Otherwise we've already been prepared
        {
            if (myText == null)
            {
                throw new SyntaxFailure(null,
                                        "null.symbol is not an expression",
                                        this);
            }

            myBinding = resolve();
            if (myBinding == null)
            {
                throw new UnboundIdentifierFailure(myText, this);
            }

            myWraps = null;
        }

        return this;
    }


    @Override
    FusionValue quote(Evaluator eval)
    {
        return eval.newSymbol(myText, getAnnotations());
    }


    @Override
    FusionValue eval(Evaluator eval, Environment env)
        throws FusionException
    {
        assert myBinding != null;
        FusionValue result = myBinding.lookup(env);
        assert result != null : "No value for " + myText;
        return result;
    }


    @Override
    void writeContentTo(IonWriter writer)
        throws IOException
    {
        writer.writeSymbol(myText);
    }
}
