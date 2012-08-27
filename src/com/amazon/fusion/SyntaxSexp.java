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
import java.util.Arrays;
import java.util.IdentityHashMap;
import java.util.List;

final class SyntaxSexp
    extends SyntaxSequence
{
    private SyntaxSexp(String[] anns, SourceLocation loc)
    {
        super(anns, loc);
    }

    /**
     * Instance will be {@link #isNullValue()} if children is null.
     *
     * @param children this instance takes ownership and it must not be modified!
     */
    private SyntaxSexp(String[] anns, SourceLocation loc,
                       List<SyntaxValue> children)
    {
        super(anns, loc, children);
    }

    static SyntaxSexp read(IonReader source, String[] anns)
    {
        SourceLocation loc = currentLocation(source);
        SyntaxSexp seq = new SyntaxSexp(anns, loc, readChildren(source));
        return seq;
    }


    /**
     * Instance will be {@link #isNullValue()} if children is null.
     */
    static SyntaxSexp make(SyntaxValue... children)
    {
        return make(null, children);
    }

    /**
     * Instance will be {@link #isNullValue()} if children is null.
     */
    static SyntaxSexp make(SourceLocation loc, SyntaxValue... children)
    {
        SyntaxSexp seq;
        if (children == null)
        {
            seq = new SyntaxSexp(EMPTY_STRING_ARRAY, loc);
            assert seq.isNullValue();
        }
        else
        {
            List<SyntaxValue> childs = Arrays.asList(children);
            seq = new SyntaxSexp(EMPTY_STRING_ARRAY, loc, childs);
        }

        return seq;
    }

    /**
     * Instance will be {@link #isNullValue()} if children is null.
     *
     * @param children this instance takes ownership and it must not be modified!
     */
    static SyntaxSexp make(SourceLocation loc, List<SyntaxValue> children)
    {
        return new SyntaxSexp(EMPTY_STRING_ARRAY, loc, children);
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
    SyntaxSexp makeSimilar(List<SyntaxValue> children)
    {
        return make(null, children);
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

        SyntaxValue[] expandedChildren = new SyntaxValue[len];

        int i = 0;
        SyntaxValue first = get(0);
        if (first instanceof SyntaxSymbol)
        {
            first = first.prepare(eval, env);
            expandedChildren[0] = first;

            // Expansion could produce something else
            if (first instanceof SyntaxSymbol)
            {
                Environment.Binding binding =
                    ((SyntaxSymbol) first).getBinding();
                FusionValue resolved = binding.lookup(env);
                if (resolved instanceof KeywordValue)
                {
                    // We found a static top-level keyword binding!
                    // Continue the preparation process.
                    // TODO tail-call
                    SyntaxValue expandedExpr =
                        ((KeywordValue)resolved).prepare(eval, env, this);
                    return expandedExpr;
                }
            }

            expandedChildren[0] = first;
            i++;  // Don't re-prepare the first subform
        }

        // else we have a procedure application, prepare each subform
        for ( ; i < len; i++)
        {
            SyntaxValue subform = get(i);
            expandedChildren[i] = subform.prepare(eval, env);
        }

        SyntaxSexp result = SyntaxSexp.make(getLocation(), expandedChildren);
        return result;
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

        SyntaxValue first = get(0); // calls pushAnyWraps()
        if (first instanceof SyntaxSymbol)
        {
            SyntaxSymbol maybeKeyword = (SyntaxSymbol) first;
            SyntaxValue prepared = maybeKeyword.prepare(eval, env);
            // Make sure we don't have to structurally change this sexp
            assert prepared == maybeKeyword;

            Environment.Binding binding = maybeKeyword.getBinding();
            if (stops.get(binding) != null)
            {
                return this;
            }

            FusionValue resolved = binding.lookup(env);
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
            // else not a macro, so just stop here.
        }

        return this;
    }

    @Override
    public FusionValue eval(Evaluator eval, Environment env)
        throws FusionException
    {
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
