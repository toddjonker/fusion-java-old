// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;

import com.amazon.fusion.Environment.Binding;
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
    /**
     * Instance will be {@link #isNullValue()} if children is null.
     *
     * @param children the children of the new sexp.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     * @param anns must not be null.
     */
    private SyntaxSexp(SyntaxValue[] children, String[] anns,
                       SourceLocation loc)
    {
        super(children, anns, loc);
    }

    /** Copy constructor shares children and replaces unpushed wraps. */
    private SyntaxSexp(SyntaxSexp that, SyntaxWraps wraps)
    {
        super(that, wraps);
    }


    /**
     * Instance will be {@link #isNullValue()} if children is null.

     * @param children the children of the new sexp.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     * @param anns must not be null.
     */
    static SyntaxSexp make(SyntaxValue[] children, String[] anns,
                           SourceLocation loc)
    {
        return new SyntaxSexp(children, anns, loc);
    }

    /**
     * Instance will be {@link #isNullValue()} if children is null.

     * @param children the children of the new sexp.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     */
    static SyntaxSexp make(SyntaxValue... children)
    {
        return make(null, children);
    }

    /**
     * Instance will be {@link #isNullValue()} if children is null.

     * @param children the children of the new sexp.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     */
    static SyntaxSexp make(SourceLocation loc, SyntaxValue... children)
    {
        return new SyntaxSexp(children, EMPTY_STRING_ARRAY, loc);
    }

    /**
     * Instance will be {@link #isNullValue()} if children is null.
     *
     * @param children this instance takes ownership of the child elements
     * and they must not be changed by calling code afterwards!
     */
    static SyntaxSexp make(SourceLocation loc, List<SyntaxValue> children)
    {
        SyntaxValue[] childs = new SyntaxValue[children.size()];
        children.toArray(childs);
        return new SyntaxSexp(childs, EMPTY_STRING_ARRAY, loc);
    }


    @Override
    SyntaxSexp copyReplacingWraps(SyntaxWraps wraps)
    {
        return new SyntaxSexp(this, wraps);
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
    SyntaxSexp makeSimilar(SyntaxValue[] children, String[] anns,
                           SourceLocation loc)
    {
        return new SyntaxSexp(children, anns, loc);
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

        SyntaxValue[] children = extract();

        SyntaxValue first = children[0];
        first = first.prepare(eval, env);
        children[0] = first;
        if (first instanceof SyntaxSymbol)
        {
            Binding binding = ((SyntaxSymbol) first).getBinding();
            FusionValue resolved = binding.lookup(env);
            if (resolved instanceof KeywordValue)
            {
                // We found a static top-level keyword binding!
                // Continue the preparation process.
                // TODO tail-call

                SyntaxSexp form =
                    SyntaxSexp.make(getLocation(), children);
                SyntaxValue expandedExpr =
                    ((KeywordValue)resolved).prepare(eval, env, form);
                return expandedExpr;
            }
        }

        // else we have a procedure application, prepare each subform
        for (int i = 1; i < len; i++)
        {
            SyntaxValue subform = children[i];
            children[i] = subform.prepare(eval, env);
        }

        SyntaxSexp result = SyntaxSexp.make(getLocation(), children);
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

            Binding binding = maybeKeyword.getBinding();
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
