// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionSexp.EMPTY_SEXP;
import static com.amazon.fusion.FusionSexp.emptySexp;
import static com.amazon.fusion.FusionSexp.nullSexp;
import static com.amazon.fusion.FusionSexp.pair;
import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import static com.amazon.fusion.FusionWrite.safeWrite;
import static com.amazon.fusion.LetValuesForm.compilePlainLet;
import com.amazon.fusion.LambdaForm.CompiledLambdaBase;
import com.amazon.fusion.LambdaForm.CompiledLambdaExact;
import com.amazon.ion.IonType;
import com.amazon.ion.IonWriter;
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
    SyntaxSexp makeSimilar(SyntaxValue[] children, String[] anns,
                           SourceLocation loc)
    {
        return new SyntaxSexp(children, anns, loc);
    }

    @Override
    void ionize(Evaluator eval, IonWriter out)
        throws IOException, FusionException
    {
        ionizeSequence(eval, out, IonType.SEXP);
    }


    //========================================================================


    @Override
    SyntaxValue doExpand(Expander expander, Environment env)
        throws FusionException
    {
        int len = size();
        if (len == 0)
        {
            throw new SyntaxFailure(null, "not a valid syntactic form", this);
        }

        SyntaxValue[] children = extract();

        SyntaxValue first = children[0];
        first = expander.expand(env, first);
        children[0] = first;
        if (first instanceof SyntaxSymbol)
        {
            Binding binding = ((SyntaxSymbol) first).getBinding();
            Object resolved = binding.lookup(env);
            if (resolved instanceof SyntacticForm)
            {
                // We found a static top-level binding to a built-in form or
                // to a macro. Continue the expansion process.

                SyntaxSexp stx =
                    SyntaxSexp.make(getLocation(), children);
                // TODO tail expand
                SyntaxValue expandedExpr =
                    expander.expand(env, (SyntacticForm) resolved, stx);
                return expandedExpr;
            }
        }

        // else we have a procedure application, expand each subform
        for (int i = 1; i < len; i++)
        {
            SyntaxValue subform = children[i];
            children[i] = expander.expand(env, subform);
        }

        SyntaxSexp result = SyntaxSexp.make(getLocation(), children);
        return result;
    }


    SyntaxValue partialExpand(Evaluator eval, Expander ctx,
                              Environment env,
                              IdentityHashMap<Binding, Object> stops)
        throws FusionException
    {
        int len = size();
        if (len == 0)
        {
            throw new SyntaxFailure(null, "not a valid syntactic form", this);
        }

        SyntaxValue first = get(0); // calls pushAnyWraps()
        if (first instanceof SyntaxSymbol)
        {
            SyntaxSymbol maybeMacro = (SyntaxSymbol) first;
            SyntaxValue prepared = ctx.expand(env, maybeMacro);
            // Make sure we don't have to structurally change this sexp
            assert prepared == maybeMacro;

            Binding binding = maybeMacro.getBinding();
            if (stops.get(binding) != null)
            {
                return this;
            }

            Object resolved = binding.lookup(env);
            if (resolved instanceof MacroTransformer)
            {
                // We found a static top-level macro binding!
                SyntaxValue expanded =
                    ((MacroTransformer)resolved).expandOnce(eval, this);
                if (expanded instanceof SyntaxSexp)
                {
                    // TODO replace recursion with iteration
                    return ((SyntaxSexp)expanded).partialExpand(eval, ctx, env,
                                                                stops);
                }
                return expanded;
            }
            // else not a macro, so just stop here.
        }

        return this;
    }



    @Override
    Object quote(Evaluator eval)
        throws FusionException
    {
        String[] annotations = getAnnotations();

        if (isNullValue())
        {
            return nullSexp(eval, annotations);
        }

        int i = size();
        if (i == 0)
        {
            return emptySexp(eval, annotations);
        }

        Object sexp = EMPTY_SEXP;
        while (i-- != 0)
        {
            SyntaxValue s = get(i);
            Object head = s.quote(eval);
            if (i == 0)
            {
                sexp = pair(eval, annotations, head, sexp);
            }
            else
            {
                sexp = pair(eval, head, sexp);
            }
        }

        return sexp;
    }


    //========================================================================


    @Override
    CompiledForm doCompile(Evaluator eval, Environment env)
        throws FusionException
    {
        SyntaxValue first = get(0);
        if (first instanceof SyntaxSymbol)
        {
            Binding binding = ((SyntaxSymbol) first).getBinding();

            // NOTE: Failure to get a binding indicates use of a built-in
            // syntactic form that's defined (probably via java_new) in the
            // same module. That's not supported! Such modules need to be
            // broken apart to meet this requirement.  This won't affect
            // users unless we open the whole compiler APIs so they can add
            // new "built-in" syntax.

            Object resolved = binding.lookup(env);
            if (resolved instanceof SyntacticForm)
            {
                // We found a static top-level syntax binding!
                // Continue the compilation process.
                // TODO bounce the tail-call?

                CompiledForm compiled =
                    ((SyntacticForm)resolved).compile(eval, env, this);
                return compiled;
            }
        }

        CompiledForm procForm = eval.compile(env, first);
        CompiledForm[] argForms = eval.compile(env, this, 1);

        if (procForm instanceof CompiledLambdaExact)
        {
            CompiledLambdaBase lambda = (CompiledLambdaBase) procForm;
            if (lambda.myArgNames.length != argForms.length)
            {
                String message =
                    "lambda form expects " + lambda.myArgNames.length +
                    " arguments but application has " + argForms.length +
                    " expressions";
                 throw new SyntaxFailure("procedure application", message,
                                         this);
            }

            return compilePlainLet(argForms, lambda.myBody);
        }

        return new CompiledPlainApp(procForm, argForms);
    }


    //========================================================================


    private static final class CompiledPlainApp
        implements CompiledForm
    {
        private final CompiledForm   myProcForm;
        private final CompiledForm[] myArgForms;

        CompiledPlainApp(CompiledForm procForm, CompiledForm[] argForms)
        {
            myProcForm = procForm;
            myArgForms = argForms;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            Object proc = eval.eval(store, myProcForm);

            int argCount = myArgForms.length;

            Object[] args;
            if (argCount == 0)
            {
                args = FusionUtils.EMPTY_OBJECT_ARRAY;
            }
            else
            {
                args = new Object[argCount];
                for (int i = 0; i < argCount; i++)
                {
                    args[i] = eval.eval(store, myArgForms[i]);
                }
            }

            Procedure p;
            try
            {
                p = (Procedure) proc;
            }
            catch (ClassCastException e)
            {
                StringBuilder b = new StringBuilder();
                b.append("Application expected procedure, given: ");
                safeWrite(eval, b, proc);
                b.append("\nArguments were: ");
                for (int i = 0; i < args.length; i++)
                {
                    b.append("\n  ");
                    safeWrite(eval, b, args[i]);
                }

                throw new FusionException(b.toString());
            }

            return eval.bounceTailCall(p, args);
        }
    }
}
