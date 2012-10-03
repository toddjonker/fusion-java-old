// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionValue.UNDEF;
import static com.amazon.fusion.FusionVector.isVector;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;
import com.amazon.ion.Timestamp;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

/**
 * Main entry point to the Fusion evaluation engine.
 */
final class Evaluator
{
    private final IonSystem mySystem;
    private final ModuleRegistry myDefaultRegistry;
    private final Evaluator myOuterFrame;
    private final Map<Object, Object> myContinuationMarks;


    Evaluator(IonSystem system, ModuleRegistry defaultRegistry)
    {
        mySystem = system;
        myDefaultRegistry = defaultRegistry;
        myOuterFrame = null;
        myContinuationMarks = null;
    }

    private Evaluator(IonSystem system, Evaluator outerBindings)
    {
        mySystem = system;
        myDefaultRegistry = outerBindings.myDefaultRegistry;
        myOuterFrame = outerBindings;
        myContinuationMarks = new HashMap<Object, Object>();
    }

    IonSystem getSystem()
    {
        return mySystem;
    }

    ModuleRegistry getModuleRegistry()
    {
        return myDefaultRegistry;
    }

    //========================================================================


    KernelModule findKernel()
    {
        return (KernelModule) myDefaultRegistry.lookup(KernelModule.IDENTITY);
    }

    /**
     * Returns an identifier (a wrapped syntax symbol) that has the bindings
     * of the {@link KernelModule} ({@code #%kernel}).
     */
    SyntaxSymbol makeKernelIdentifier(String symbol)
    {
        SyntaxSymbol sym = SyntaxSymbol.make(symbol);
        sym = sym.addWrap(new ModuleRenameWrap(findKernel()));
        return sym;
    }


    Namespace newNamespaceWithLanguage(ModuleRegistry registry,
                                       String modulePath)
        throws FusionException
    {
        Namespace ns = new Namespace(registry);
        ns.use(this, modulePath);
        return ns;
    }


    /**
     * Creates a new namespace sharing a namespace's registry.
     *
     * @param ns carries the {@link ModuleRegistry} to share.
     * Must not be null.
     */
    Namespace newEmptyNamespace(Namespace ns)
    {
        ModuleRegistry registry = ns.getRegistry();
        return new Namespace(registry);
    }


    private Namespace newBaseNamespace(ModuleRegistry registry)
        throws FusionException
    {
        return newNamespaceWithLanguage(registry, "fusion/base");
    }


    /**
     * Creates a new namespace sharing this {@link Evaluator}'s default
     * registry and {@code use}ing {@code fusion/base}.
     */
    Namespace newBaseNamespace()
        throws FusionException
    {
        return newBaseNamespace(myDefaultRegistry);
    }


    /**
     * Creates a new namespace sharing a namespace's registry
     * and {@code use}ing {@code fusion/base}.
     *
     * @param ns carries the {@link ModuleRegistry} to share.
     * Must not be null.
     */
    Namespace newBaseNamespace(Namespace ns)
        throws FusionException
    {
        ModuleRegistry registry = ns.getRegistry();
        return newBaseNamespace(registry);
    }

    //========================================================================

    /**
     * Injects an Ion DOM into the equivalent Fusion runtime objects.
     * It is an error for modifications to be made to the argument instance
     * (or anything it refers to) after it is passed to this method.
     *
     * @param value may be null, meaning {@code null.null}.
     */
    Object inject(IonValue value)
    {
        if (value == null)
        {
            value = mySystem.newNull();
        }
        return value;
    }


    /**
     * Transforms a Java value to a Fusion value, where possible.
     * It is an error for modifications to be made to the argument instance
     * (or anything it refers to) after it is passed to this method.
     *
     * @param javaValue may be null, meaning {@code null.null}.
     *
     * @return the injected value.
     *
     * @throws IllegalArgumentException if the value isn't acceptable to the
     * Fusion runtime.
     */
    Object inject(Object javaValue)
    {
        if (javaValue == null || javaValue instanceof IonValue)
        {
            return inject((IonValue) javaValue);
        }

        if (javaValue instanceof FusionValue)
        {
            return javaValue;
        }

        if (javaValue instanceof Object[])
        {
            // TODO recurse through array?
            return FusionVector.makeVectorFrom(this, (Object[]) javaValue);
        }

        String message = "Unacceptable type: " + javaValue.getClass();
        throw new IllegalArgumentException(message);
    }


    //========================================================================


    Object newNull(String... annotations)
    {
        IonValue dom = mySystem.newNull();
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return inject(dom);
    }

    Object newBool(boolean value)
    {
        IonValue dom = mySystem.newBool(value);
        return inject(dom);
    }

    Object newBool(boolean value, String... annotations)
    {
        IonValue dom = mySystem.newBool(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return inject(dom);
    }

    Object newBool(Boolean value)
    {
        IonValue dom = mySystem.newBool(value);
        return inject(dom);
    }

    Object newBool(Boolean value, String... annotations)
    {
        IonValue dom = mySystem.newBool(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return inject(dom);
    }

    Object newInt(long value, String... annotations)
    {
        IonValue dom = mySystem.newInt(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return inject(dom);
    }

    Object newInt(BigInteger value, String... annotations)
    {
        IonValue dom = mySystem.newInt(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return inject(dom);
    }

    Object newString(String value, String... annotations)
    {
        IonValue dom = mySystem.newString(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return inject(dom);
    }

    Object newSymbol(String value, String... annotations)
    {
        IonValue dom = mySystem.newSymbol(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return inject(dom);
    }

    Object newDecimal(BigDecimal value, String... annotations)
    {
        IonValue dom = mySystem.newDecimal(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return inject(dom);
    }

    Object newDecimal(double value, String... annotations)
    {
        IonValue dom = mySystem.newDecimal(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return inject(dom);
    }

    Object newFloat(double value)
    {
        IonValue dom = mySystem.newFloat(value);
        return inject(dom);
    }

    Object newFloat(double value, String... annotations)
    {
        IonValue dom = mySystem.newFloat(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return inject(dom);
    }

    Object newFloat(Double value)
    {
        IonValue dom = (value == null
                           ? mySystem.newNullFloat()
                           : mySystem.newFloat(value));
        return inject(dom);
    }

    Object newFloat(Double value, String... annotations)
    {
        IonValue dom = (value == null
                           ? mySystem.newNullFloat()
                           : mySystem.newFloat(value));
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return inject(dom);
    }

    Object newTimestamp(Timestamp value, String... annotations)
    {
        IonValue dom = mySystem.newTimestamp(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return inject(dom);
    }

    Object newBlob(byte[] value, String... annotations)
    {
        IonValue dom = mySystem.newBlob(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return inject(dom);
    }

    Object newClob(byte[] value, String... annotations)
    {
        IonValue dom = mySystem.newClob(value);
        if (annotations != null) dom.setTypeAnnotations(annotations);
        return inject(dom);
    }


    //========================================================================


    /**
     * Casts or copies as necessary. Throws if vector contains non-Ion data!
     *
     * @deprecated This is of questionable design, and should probably be
     * removed. Please limit use carefully.
     */
    @Deprecated
    IonValue convertToIonValueMaybe(Object value)
        throws FusionException
    {
        if (value instanceof IonValue)
        {
            return (IonValue) value;
        }

        if (isVector(value))
        {
            return FusionVector.unsafeCopyToIonList(value, mySystem);
        }

        return null;
    }


    //========================================================================

    // This is a shady implementation of Racket's continuation marks.
    // It's not full featured: we don't create every continuation frame, so we
    // can't implement the primitive with-continuation-mark.

    Object firstContinuationMark(Object key)
    {
        // The keys must be hashable and equals-able!
        assert key instanceof DynamicParameter;

        Evaluator e = this;
        while (e.myOuterFrame != null)
        {
            Object value = e.myContinuationMarks.get(key);
            if (value != null) return value;
            e = e.myOuterFrame;
        }
        return null;
    }


    Evaluator markedContinuation(Object key, Object mark)
    {
        // The keys must be hashable and equals-able!
        assert key instanceof DynamicParameter;

        Evaluator innerFrame = new Evaluator(mySystem, this);
        innerFrame.myContinuationMarks.put(key, mark);
        return innerFrame;
    }

    Evaluator markedContinuation(Object[] keys, Object[] marks)
    {
        assert keys.length == marks.length;

        Evaluator innerFrame = new Evaluator(mySystem, this);
        for (int i = 0; i < keys.length; i++)
        {
            // The keys must be hashable and equals-able!
            assert keys[i] instanceof DynamicParameter;

            innerFrame.myContinuationMarks.put(keys[i], marks[i]);
        }
        return innerFrame;
    }


    //========================================================================

    Namespace findCurrentNamespace()
    {
        DynamicParameter param = findKernel().getCurrentNamespaceParameter();
        return (Namespace) param.currentValue(this);
    }


    private Evaluator parameterizeCurrentNamespace(Namespace ns)
    {
        if (ns == null) return this;

        DynamicParameter param = findKernel().getCurrentNamespaceParameter();
        return markedContinuation(param, ns);
    }


    /**
     * Calls the current evaluation handler, evaluating the given source
     * within the current namespace.
     *
     * @see <a href="http://docs.racket-lang.org/reference/eval.html#%28def._%28%28quote._~23~25kernel%29._current-eval%29%29">
         Racket's <code>eval</code></a>
     */
    Object callCurrentEval(SyntaxValue source)
        throws FusionException
    {
        Namespace ns = findCurrentNamespace();

        // TODO this should partial-expand and splice begins

        source = expand(ns, source);
        CompiledForm compiled = compile(ns, source);
        source = null; // Don't hold garbage

        return eval(ns, compiled);
    }


    /**
     * Expands, compiles, and evaluates a single top-level form.
     * <p>
     * Equivalent to Racket's {@code eval} (but incomplete at the moment.)
     *
     * @param ns may be null to use {@link #findCurrentNamespace()}.
     */
    Object eval(SyntaxValue source, Namespace ns)
        throws FusionException
    {
        Evaluator eval = parameterizeCurrentNamespace(ns);

        // TODO FUSION-44 handle (module ...) properly
        source = eval.findCurrentNamespace().syntaxIntroduce(source);

        // TODO this should be a tail call
        return eval.callCurrentEval(source);
    }


    /**
     * Like {@link #eval(SyntaxValue, Namespace)},
     * but does not enrich the source's lexical context.
     *
     * @param ns may be null to use {@link #findCurrentNamespace()}.
     */
    Object evalSyntax(SyntaxValue source, Namespace ns)
        throws FusionException
    {
        Evaluator eval = parameterizeCurrentNamespace(ns);

        // TODO this should be a tail call
        return eval.callCurrentEval(source);
    }


    // TODO FUSION-43 expansion should recur through here
    SyntaxValue expand(Environment env, SyntaxValue source)
        throws SyntaxFailure
    {
        // TODO FUSION-43 Fail if there are annotations
        return source.expand(this, env);
    }


    CompiledForm compile(Environment env, SyntaxValue source)
        throws FusionException
    {
        return source.doCompile(this, env);
    }

    /**
     * @return not null, but perhaps {@link CompiledForm#EMPTY_ARRAY}.
     */
    CompiledForm[] compile(Environment env, SyntaxSequence source,
                           int from, int to)
        throws FusionException
    {
        int size = to - from;

        if (size == 0) return CompiledForm.EMPTY_ARRAY;

        CompiledForm[] forms = new CompiledForm[size];
        for (int i = from; i < to; i++)
        {
            SyntaxValue form = source.get(i);
            forms[i - from] = compile(env, form);
        }

        return forms;
    }

    /**
     * @return not null, but perhaps {@link CompiledForm#EMPTY_ARRAY}.
     */
    CompiledForm[] compile(Environment env, SyntaxSequence source, int from)
        throws FusionException
    {
        return compile(env, source, from, source.size());
    }


    /**
     * @return not null
     */
    Object eval(Store store, CompiledForm form)
        throws FusionException
    {
        moreEval: while (true)
        {
            /*
            if (expr.getAnnotations().length != 0)
            {
                String message =
                    "Annotations not supported in raw syntax. You probably " +
                    "want to quote this value";
                throw new SyntaxFailure(null, message, expr);
            }
*/

            Object result = form.doEval(this, store);
            while (true)
            {
                if (result == null)
                {
                    return UNDEF;
                }
                if (result instanceof TailForm)
                {
                    TailForm tail = (TailForm) result;
                    store = tail.myStore;
                    form  = tail.myForm;
                    continue moreEval;
                }
                if (result instanceof TailCall)
                {
                    TailCall tail = (TailCall) result;
                    result = tail.myProc.doApply(this, tail.myArgs);
                    continue;
                }
                return result;
            }
        }
    }


    /**
     * Makes a <b>non-tail</b> procedure call.
     * Whenever possible, you should use tail calls instead.
     *
     * @return not null
     *
     * @see #bounceTailCall(Procedure, Object...)
     */
    Object callNonTail(Procedure proc, Object... args)
        throws FusionException
    {
        Object result = proc.doApply(this, args);
        if (result instanceof TailForm)
        {
            TailForm tail = (TailForm) result;
            result = eval(tail.myStore, tail.myForm);
        }
        // TODO handle TailCall, but nothing triggers this path yet.
        else if (result == null)
        {
            result = UNDEF;
        }
        return result;
    }


    //========================================================================


    /**
     * Wraps an expression for evaluation in tail position.
     * Must be returned back to this {@link Evaluator} for proper behavior.
     */
    Object bounceTailForm(Store store, CompiledForm form)
    {
        return new TailForm(store, form);
    }


    /**
     * Returned from evaluation of a form when evaluation needs to continue in
     * a tail position. This allows the {@link Evaluator} to trampoline into
     * the tail call without growing the stack.  Not the most efficient
     * implementation, but it works.
     */
    private static final class TailForm
        extends FusionValue
    {
        final Store        myStore;
        final CompiledForm myForm;

        TailForm(Store store, CompiledForm form)
        {
            myStore = store;
            myForm  = form;
        }

        @Override
        public void write(Appendable out)
        {
            throw new IllegalStateException();
        }
    }


    /**
     * Makes a procedure call from tail position.
     * The result MUST be immediately returned to the evaluator,
     * it's not a normal value!
     *
     * @return not null
     */
    Object bounceTailCall(Procedure proc, Object... args)
        throws FusionException
    {
        return new TailCall(proc, args);

    }

    /**
     * Returned from evaluation of a form when evaluation needs to continue in
     * a tail position. This allows the {@link Evaluator} to trampoline into
     * the tail call without growing the stack.  Not the most efficient
     * implementation, but it works.
     */
    private static final class TailCall
        extends FusionValue
    {
        final Procedure myProc;
        final Object[]  myArgs;

        TailCall(Procedure proc, Object... args)
        {
            myProc = proc;
            myArgs = args;
        }

        @Override
        public void write(Appendable out)
        {
            throw new IllegalStateException();
        }
    }
}
