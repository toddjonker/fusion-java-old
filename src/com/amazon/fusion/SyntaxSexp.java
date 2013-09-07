// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionList.unsafeListToSexp;
import static com.amazon.fusion.FusionSexp.immutableSexp;
import static com.amazon.fusion.FusionSexp.isEmptySexp;
import static com.amazon.fusion.FusionSexp.isNullSexp;
import static com.amazon.fusion.FusionSexp.isPair;
import static com.amazon.fusion.FusionSexp.isSexp;
import static com.amazon.fusion.FusionSexp.nullSexp;
import static com.amazon.fusion.FusionSexp.pair;
import static com.amazon.fusion.FusionSexp.unsafePairDot;
import static com.amazon.fusion.FusionSexp.unsafePairHead;
import static com.amazon.fusion.FusionSexp.unsafePairTail;
import static com.amazon.fusion.FusionSexp.unsafeSexpSize;
import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import static com.amazon.fusion.FusionWrite.safeWrite;
import static com.amazon.fusion.LetValuesForm.compilePlainLet;
import com.amazon.fusion.FusionSexp.BaseSexp;
import com.amazon.fusion.FusionSexp.ImmutablePair;
import com.amazon.fusion.LambdaForm.CompiledLambdaBase;
import com.amazon.fusion.LambdaForm.CompiledLambdaExact;
import com.amazon.ion.IonWriter;
import java.io.IOException;
import java.util.IdentityHashMap;
import java.util.List;

final class SyntaxSexp
    extends SyntaxSequence
{
    private BaseSexp mySexp;


    /**
     * @param sexp must not be null.
     */
    private SyntaxSexp(Evaluator eval, SourceLocation loc, BaseSexp sexp)
    {
        super(loc, sexp.myAnnotations);
        assert eval != null;
        mySexp = sexp;
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
    private SyntaxSexp(Evaluator eval, SourceLocation loc, String[] anns,
                       SyntaxValue[] children)
    {
        super(loc, anns);
        assert eval != null;
        mySexp = (children == null
                      ? nullSexp(eval, anns)
                      : immutableSexp(eval, anns, children));
    }

    /** Copy constructor shares children and replaces unpushed wraps. */
    private SyntaxSexp(SyntaxSexp that, SyntaxWraps wraps)
    {
        super(that.getAnnotations(), that.getLocation(), wraps);
        mySexp = that.mySexp;
    }


    static SyntaxSexp make(Evaluator eval, SourceLocation loc, BaseSexp sexp)
    {
        return new SyntaxSexp(eval, loc, sexp);
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


    //========================================================================


    private static ImmutablePair pushWraps(Evaluator eval,
                                           ImmutablePair pair,
                                           SyntaxWraps wraps)
        throws FusionException
    {
        Object head = unsafePairHead(eval, pair);
        Object tail = unsafePairTail(eval, pair);

        Object newHead = head;
        if (head instanceof SyntaxValue)
        {
            newHead = ((SyntaxValue) head).addWraps(wraps);
        }

        Object newTail = tail;
        if (isPair(eval, tail))
        {
            newTail = pushWraps(eval, (ImmutablePair) tail, wraps);
        }
        else if (tail instanceof SyntaxValue)
        {
            newTail = ((SyntaxValue) tail).addWraps(wraps);
        }

        if (head != newHead || tail != newTail)
        {
            pair = pair(eval, pair.myAnnotations, newHead, newTail);
        }

        return pair;
    }


    /**
     * If we have wraps cached here, push them down into fresh copies of all
     * children. This must be called before exposing any children outside of
     * this instance, so that it appears as if the wraps were pushed when they
     * were created.
     */
    private void pushWraps(Evaluator eval)
        throws FusionException
    {
        if (myWraps != null)  // We only have wraps when we have children.
        {
            mySexp = pushWraps(eval, (ImmutablePair) mySexp, myWraps);
            myWraps = null;
        }
    }


    @Override
    SyntaxValue[] extract(Evaluator eval)
        throws FusionException
    {
        pushWraps(eval);

        if (isNullSexp(eval, mySexp)) return null;

        int len = unsafeSexpSize(eval, mySexp);
        SyntaxValue[] extracted = new SyntaxValue[len];

        int i = 0;
        for (Object p = mySexp; isPair(eval, p); p = unsafePairTail(eval, p))
        {
            extracted[i] = (SyntaxValue) unsafePairHead(eval, p);
            i++;
        }

        return extracted;
    }


    void extract(Evaluator eval, List<SyntaxValue> list, int from)
        throws FusionException
    {
        pushWraps(eval);

        int i = 0;
        for (Object p = mySexp; isPair(eval, p); p = unsafePairTail(eval, p))
        {
            if (from <= i)
            {
                SyntaxValue child = (SyntaxValue) unsafePairHead(eval, p);
                list.add(child);
            }
            i++;
        }
    }


    private static ImmutablePair stripWraps(Evaluator eval,
                                            ImmutablePair pair)
        throws FusionException
    {
        Object head = unsafePairHead(eval, pair);
        Object tail = unsafePairTail(eval, pair);

        Object newHead = head;
        if (head instanceof SyntaxValue)
        {
            newHead = ((SyntaxValue) head).stripWraps(eval);
        }

        Object newTail = tail;
        if (isPair(eval, tail))
        {
            newTail = stripWraps(eval, (ImmutablePair) tail);
        }
        else if (tail instanceof SyntaxValue)
        {
            newTail = ((SyntaxValue) tail).stripWraps(eval);
        }

        if (head != newHead || tail != newTail)
        {
            pair = pair(eval, pair.myAnnotations, newHead, newTail);
        }

        return pair;
    }

    @Override
    SyntaxSequence stripWraps(Evaluator eval)
        throws FusionException
    {
        if (hasNoChildren()) return this;  // No children, no marks, all okay!

        BaseSexp newSexp = stripWraps(eval, (ImmutablePair) mySexp);
        return new SyntaxSexp(eval, getLocation(), newSexp);
    }


    @Override
    SyntaxSexp copyReplacingWraps(SyntaxWraps wraps)
        throws FusionException
    {
        assert ! hasNoChildren() && wraps != null;
        return new SyntaxSexp(this, wraps);
    }


    @Override
    Type getType()
    {
        return Type.SEXP;
    }


    @Override
    boolean isNullValue()
    {
        return mySexp.isAnyNull();
    }


    @Override
    boolean hasNoChildren()
        throws FusionException
    {
        return ! (mySexp instanceof ImmutablePair);
    }


    @Override
    int size()
        throws FusionException
    {
        return mySexp.size();
    }


    @Override
    SyntaxValue get(Evaluator eval, int index)
        throws FusionException
    {
        pushWraps(eval);
        return (SyntaxValue) unsafePairDot(eval, mySexp, index);
    }


    @Override
    void ionize(Evaluator eval, IonWriter out)
        throws IOException, FusionException
    {
        // Ionization doesn't require wraps to be pushed.
        mySexp.ionize(eval, out);
    }


    //========================================================================
    // Helpers for quote, syntax_to_datum, and syntax_unwrap


    private static Object unwrap(Evaluator eval, BaseSexp sexp)
        throws FusionException
    {
        if (isPair(eval, sexp))
        {
            Object head = unsafePairHead(eval, sexp);
            head = ((SyntaxValue) head).unwrap(eval, true);

            Object tail = unsafePairTail(eval, sexp);
            if (isSexp(eval, tail))
            {
                tail = unwrap(eval, (BaseSexp) tail);
            }
            else
            {
                tail = ((SyntaxValue) tail).unwrap(eval, true);
            }

            sexp = pair(eval, sexp.myAnnotations, head, tail);
        }

        return sexp;
    }


    @Override
    Object unwrap(Evaluator eval, boolean recurse)
        throws FusionException
    {
        pushWraps(eval);

        if (recurse)
        {
            return unwrap(eval, mySexp);
        }

        return mySexp;
    }


    //========================================================================
    // Helpers for syntax_append and syntax_subseq


    private static BaseSexp append(Evaluator eval, Object front, BaseSexp back)
        throws FusionException
    {
        if (isPair(eval, front))
        {
            Object tail = unsafePairTail(eval, front);
            tail = append(eval, tail, back);
            if (tail != null)
            {
                Object head = unsafePairHead(eval, front);
                return FusionSexp.pair(eval, head, tail);
            }
        }
        else if (isEmptySexp(eval, front))
        {
            return back;
        }

        return null;
    }


    @Override
    SyntaxSequence makeAppended(Evaluator eval, SyntaxSequence that)
        throws FusionException
    {
        Object back = that.unwrap(eval, false);

        BaseSexp backSexp;
        if (that instanceof SyntaxSexp)
        {
            backSexp = (BaseSexp) back;
        }
        else
        {
            backSexp = unsafeListToSexp(eval, back);
        }

        pushWraps(eval);

        BaseSexp appended = append(eval, mySexp, backSexp);
        if (appended == null)
        {
            return null;
        }

        return SyntaxSexp.make(eval, null, appended);
    }


    /**
     * @return null if not a proper sexp and we go beyond the end.
     */
    private static Object subseq(Evaluator eval, Object sexp, int from)
        throws FusionException
    {
        if (from == 0) return sexp;

        if (isEmptySexp(eval, sexp))
        {
            return sexp;
        }

        if (isPair(eval, sexp))
        {
            Object tail = unsafePairTail(eval, sexp);
            return subseq(eval, tail, from - 1);
        }

        return null;
    }

    @Override
    SyntaxSequence makeSubseq(Evaluator eval, int from)
        throws FusionException
    {
        pushWraps(eval);

        BaseSexp sub = (BaseSexp) subseq(eval, mySexp, from);

        return make(eval, null, sub);
    }



    //========================================================================

    /**
     * Finds the binding for the leading symbol in the sexp, or null if the
     * sexp doesn't start with a symbol.
     */
    Binding firstBinding(Evaluator eval)
        throws FusionException
    {
        if (isPair(eval, mySexp))
        {
            pushWraps(eval);

            Object first = unsafePairHead(eval, mySexp);
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
            throw new SyntaxException(null, message, this);
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
            String message =
                "not a valid syntactic form. You probably want to quote this.";
            throw new SyntaxException(null, message, this);
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
                 throw new SyntaxException("procedure application", message,
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
