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

final class SyntaxSexp
    extends SyntaxSequence
{
    /**
     * Instance will be {@link #isNullValue()} if children is null.
     *
     * @param anns must not be null.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     * @param children the children of the new sexp.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     */
    private SyntaxSexp(Evaluator eval, SourceLocation loc, String[] anns,
                       SyntaxValue[] children)
    {
        super(loc, anns, children);
        assert eval != null;
    }

    /** Copy constructor shares children and replaces unpushed wraps. */
    private SyntaxSexp(SyntaxSexp that, SyntaxWraps wraps)
    {
        super(that, wraps);
    }


    /**
     * Instance will be {@link #isNullValue()} if children is null.
     *
     * @param anns must not be null.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     * @param children the children of the new sexp.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     */
    static SyntaxSexp make(Evaluator eval,
                           SourceLocation loc,
                           String[] anns,
                           SyntaxValue[] children)
    {
        return new SyntaxSexp(eval, loc, anns, children);
    }

    /**
     * Instance will be {@link #isNullValue()} if children is null.

     * @param children the children of the new sexp.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     */
    static SyntaxSexp make(Evaluator eval, SyntaxValue... children)
    {
        return new SyntaxSexp(eval, null, EMPTY_STRING_ARRAY, children);
    }

    /**
     * Instance will be {@link #isNullValue()} if children is null.

     * @param children the children of the new sexp.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     */
    static SyntaxSexp make(Evaluator eval, SourceLocation loc,
                           SyntaxValue... children)
    {
        return new SyntaxSexp(eval, loc, EMPTY_STRING_ARRAY, children);
    }

    /**
     * Instance will be {@link #isNullValue()} if children is null.

     * @param children the children of the new sexp.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     */
    static SyntaxSexp make(Expander expander, SyntaxValue... children)
    {
        return new SyntaxSexp(expander.getEvaluator(), null,
                              EMPTY_STRING_ARRAY, children);
    }

    /**
     * Instance will be {@link #isNullValue()} if children is null.

     * @param children the children of the new sexp.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     */
    static SyntaxSexp make(Expander expander, SourceLocation loc,
                           SyntaxValue... children)
    {
        return new SyntaxSexp(expander.getEvaluator(), loc,
                              EMPTY_STRING_ARRAY, children);
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
    SyntaxSexp makeSimilar(Evaluator eval, SourceLocation loc,
                           String[] anns,
                           SyntaxValue[] children)
    {
        return new SyntaxSexp(eval, loc, anns, children);
    }

    @Override
    void ionize(Evaluator eval, IonWriter out)
        throws IOException, FusionException
    {
        ionizeSequence(eval, out, IonType.SEXP);
    }


    //========================================================================

    /**
     * Finds the binding for the leading symbol in the sexp, or null if the
     * sexp doesn't start with a symbol.
     */
    Binding firstBinding(Evaluator eval)
        throws FusionException
    {
        if (size() != 0)
        {
            SyntaxValue first = get(eval, 0);
            if (first instanceof SyntaxSymbol)
            {
                Binding binding = ((SyntaxSymbol)first).uncachedResolve();
                return binding.originalBinding();
            }
        }
        return null;
    }


    @Override
    SyntaxValue doExpand(Expander expander, Environment env)
        throws FusionException
    {
        int len = size();
        if (len == 0)
        {
            String message =
                "not a valid syntactic form. You probably want to quote this.";
            throw new SyntaxFailure(null, message, this);
        }

        SyntaxValue[] children = extract(expander.getEvaluator());

        SyntaxValue first = children[0];
        if (first instanceof SyntaxSymbol)
        {
            // Do not assume that the symbol will be treated as a variable,
            // since this scope may override #%app or #%variable-reference.
            // Thus we cannot call expand on the symbol and must not cache the
            // results of the binding resolution unless it resolves to syntax.
            SyntacticForm xform =
                ((SyntaxSymbol) first).resolveSyntaxMaybe(env);
            if (xform != null)
            {
                // We found a static top-level binding to a built-in form or
                // to a macro. Continue the expansion process.

                // TODO FUSION-31 identifier macros entail extra work here.
                assert expander.expand(env, first) == first;
                // else the next stmt must change

                // TODO tail expand

                // We use the same expansion context as we already have.
                // Don't need to replace the sexp since we haven't changed it.
                SyntaxValue expandedExpr =
                    expander.expand(env, xform, this);
                return expandedExpr;
            }
        }

        // else we have a procedure application, expand each subform as an
        // expression
        for (int i = 0; i < len; i++)
        {
            SyntaxValue subform = children[i];
            children[i] = expander.expandExpression(env, subform);
        }

        SyntaxSexp result = SyntaxSexp.make(expander, getLocation(), children);
        return result;
    }


    /**
     * This is actually an incomplete implementation of something like Racket's
     * {@code local-expand}. Partial expansion is defined based on core syntax
     * forms, not on a given stop-list.
     *
     * @see Expander#partialExpand(Environment, SyntaxValue)
     *
     * @deprecated No longer in use but I'm not ready to delete it.
     */
    @Deprecated
    final SyntaxValue partialExpand(Expander expander, Environment env,
                                    IdentityHashMap<Binding, Object> stops)
        throws FusionException
    {
        int len = size();
        if (len == 0)
        {
            throw new SyntaxFailure(null, "not a valid syntactic form", this);
        }

        Evaluator eval = expander.getEvaluator();
        SyntaxValue first = get(eval, 0); // calls pushAnyWraps()
        if (first instanceof SyntaxSymbol)
        {
            SyntaxSymbol maybeMacro = (SyntaxSymbol) first;
            SyntaxValue prepared = expander.expand(env, maybeMacro);

            // Identifier has been expanded to #%top, we can stop.
            if (prepared != maybeMacro) return this;

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
                    ((MacroTransformer)resolved).expandOnce(expander, this);
                if (expanded instanceof SyntaxSexp)
                {
                    // TODO replace recursion with iteration
                    return ((SyntaxSexp)expanded).partialExpand(expander, env,
                                                                stops);
                }
                return expanded;
            }
            // else not a macro, so just stop here.
        }

        return this;
    }



    @Override
    Object unwrap(Evaluator eval, boolean recurse)
        throws FusionException
    {
        String[] annotations = getAnnotations();

        if (isNullValue())
        {
            return nullSexp(eval, annotations);
        }

        int size = size();
        if (size == 0)
        {
            return emptySexp(eval, annotations);
        }

        if (recurse)
        {
            Object sexp = EMPTY_SEXP;
            for (int i = size; i-- != 0;)
            {
                SyntaxValue s = get(eval, i);
                Object head = s.unwrap(eval, true);
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
        Object head = get(eval, 0);
        Object tail = (size == 1 ? EMPTY_SEXP : makeSubseq(eval, 1, size));
        return pair(eval, annotations, head, tail);
    }


    //========================================================================


    @Override
    CompiledForm doCompile(Evaluator eval, Environment env)
        throws FusionException
    {
        SyntaxValue first = get(eval, 0);
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
                    "procedure expects " + lambda.myArgNames.length +
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
