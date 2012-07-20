// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import com.amazon.ion.IonType;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
import java.io.IOException;

final class SyntaxSexp
    extends SyntaxSequence
{
    private SyntaxSexp() { }

    static SyntaxSexp read(IonReader source)
    {
        SyntaxSexp seq = new SyntaxSexp();
        seq.readChildren(source);
        return seq;
    }

    static SyntaxSexp makeEmpty()
    {
        SyntaxSexp seq = new SyntaxSexp();
        seq.ensureNotNull();
        return seq;
    }

    static SyntaxSexp make(SyntaxValue... children)
    {
        SyntaxSexp seq = new SyntaxSexp();
        seq.add(children);
        return seq;
    }


    @Override
    Type getType()
    {
        return Type.SEXP;
    }


    @Override
    FusionValue quote(Evaluator eval)
    {
        ValueFactory vf = eval.getSystem();
        return addQuotedChildren(eval, vf, vf.newNullSexp());
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
