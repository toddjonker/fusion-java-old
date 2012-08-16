// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import static com.amazon.fusion.SourceLocation.currentLocation;
import com.amazon.fusion.Environment.Binding;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonSequence;
import com.amazon.ion.IonType;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
import java.io.IOException;
import java.util.IdentityHashMap;
import java.util.List;

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

    static SyntaxSexp make(List<? extends SyntaxValue> children)
    {
        SyntaxSexp seq = new SyntaxSexp(EMPTY_STRING_ARRAY, null);
        seq.addAll(children);
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
    SyntaxValue prepare(Evaluator eval, Environment env)
        throws SyntaxFailure
    {
        int len = size();
        if (len == 0)
        {
            throw new SyntaxFailure(null, "not a valid syntactic form", this);
        }

        int i = 0;
        SyntaxValue first = myChildren.get(0);
        if (first instanceof SyntaxSymbol)
        {
            SyntaxSymbol maybeKeyword = (SyntaxSymbol) first; // TODO remove
            SyntaxValue expanded = maybeKeyword.prepare(eval, env);
            if (expanded != maybeKeyword) myChildren.set(0, expanded);

            // Expansion could produce something else
            if (expanded instanceof SyntaxSymbol)
            {
                String text = ((SyntaxSymbol)expanded).stringValue();
                // TODO what if the binding is to a different namespace?
                FusionValue resolved = env.lookup(text);
                if (resolved instanceof KeywordValue)
                {
                    // We found a static top-level keyword binding!
                    return ((KeywordValue)resolved).prepare(eval, env, this);
                }
            }

            i++;  // Don't re-prepare the first subform
        }

        // else we have a procedure application, prepare each subform
        for ( ; i < len; i++)
        {
            SyntaxValue subform = myChildren.get(i);
            SyntaxValue expanded = subform.prepare(eval, env);
            if (expanded != subform) myChildren.set(i, expanded);
        }

        return this;
    }


    SyntaxValue partialExpand(Evaluator eval, Environment env,
                              IdentityHashMap<Binding, Object> stops)
        throws SyntaxFailure
    {
        int len = size();
        if (len == 0)
        {
            throw new SyntaxFailure(null, "not a valid syntactic form", this);
        }

        SyntaxValue first = get(0);
        if (first instanceof SyntaxSymbol)
        {
            SyntaxSymbol maybeKeyword = (SyntaxSymbol) first;
            SyntaxValue prepared = maybeKeyword.prepare(eval, env);
            // Make sure we don't have to structurally change this sexp
            assert prepared == maybeKeyword;
            if (stops.get(maybeKeyword.getBinding()) != null)
            {
                return this;
            }

            FusionValue resolved = env.lookup(maybeKeyword.stringValue());
            if (resolved instanceof MacroTransformer)
            {
                // We found a static top-level keyword binding!
                SyntaxValue expanded =
                    ((MacroTransformer)resolved).expandOnce(eval, env, this);
                if (expanded instanceof SyntaxSexp)
                {
                    // TODO replace recursion with iteration
                    return ((SyntaxSexp)expanded).partialExpand(eval, env,
                                                                stops);
                }
                return expanded;
            }
        }

        return this;
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
    void writeContentTo(IonWriter writer)
        throws IOException
    {
        writeContentTo(writer, IonType.SEXP);
    }
}
