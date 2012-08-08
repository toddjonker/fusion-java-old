// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import static com.amazon.fusion.SourceLocation.currentLocation;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonSequence;
import com.amazon.ion.IonType;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
import java.io.IOException;

final class SyntaxSexp
    extends SyntaxSequence
{
    private SyntaxSexp(String[] anns, SourceLocation loc)
    {
        super(anns, loc);
    }

    static SyntaxSexp read(IonReader source, String[] anns)
    {
        SourceLocation loc = currentLocation(source);
        SyntaxSexp seq = new SyntaxSexp(anns, loc);
        seq.readChildren(source);
        return seq;
    }

    static SyntaxSexp makeEmpty()
    {
        SyntaxSexp seq = new SyntaxSexp(EMPTY_STRING_ARRAY, null);
        seq.ensureNotNull();
        return seq;
    }

    static SyntaxSexp make(SyntaxValue... children)
    {
        SyntaxSexp seq = new SyntaxSexp(EMPTY_STRING_ARRAY, null);
        seq.add(children);
        return seq;
    }


    @Override
    Type getType()
    {
        return Type.SEXP;
    }


    @Override
    IonSequence makeNull(ValueFactory factory)
    {
        return factory.newNullSexp();
    }


    @Override
    public FusionValue eval(Evaluator eval, Environment env)
        throws FusionException
    {
        int len = size();
        if (len == 0)
        {
            throw new SyntaxFailure(null, "not an expression", this);
        }

        SyntaxValue first = get(0);

        FusionValue form = eval.eval(env, first);
        FusionValue result = form.invoke(eval, env, this);
        return result;
    }


    @Override
    void writeTo(IonWriter writer)
        throws IOException
    {
        writeTo(writer, IonType.SEXP);
    }
}
