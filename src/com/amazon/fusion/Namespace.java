// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionValue.UNDEF;
import static com.amazon.fusion.FusionValue.writeToString;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Set;

/**
 * Expand- and compile-time environment for all top-level sequences, and
 * eval-time store for non-module top levels.
 */
class Namespace
    implements Environment, NamespaceStore
{
    static class TopBinding implements Binding
    {
        private final SyntaxSymbol myIdentifier;
        final int myAddress;

        TopBinding(SyntaxSymbol identifier, int address)
        {
            assert identifier.resolve() instanceof FreeBinding;
            myIdentifier = identifier;
            myAddress = address;
        }

        @Override
        public final String getName()
        {
            return myIdentifier.stringValue();
        }

        @Override
        public Binding originalBinding()
        {
            return this;
        }

        @Override
        public FusionValue lookup(Environment env)
        {
            // Skip over any lexical environments and go straight to the top.
            return env.namespace().lookup(this);
        }

        @Override
        public CompiledForm compileReference(Evaluator eval, Environment env)
            throws FusionException
        {
            return new CompiledTopVariableReference(myAddress);
        }

        CompiledForm compileDefine(Evaluator eval,
                                   Environment env,
                                   CompiledForm valueForm)
        {
            String name = getName();
            return new CompiledTopDefine(name, myAddress, valueForm);
        }

        CompiledForm compileDefineSyntax(Evaluator eval,
                                         Environment env,
                                         CompiledForm valueForm)
        {
            String name = getName();
            return new CompiledTopDefineSyntax(name, myAddress, valueForm);
        }

        @Override
        public boolean equals(Object other)
        {
            return this == other;
        }

        @Override
        public String toString()
        {
            return "{{TopBinding " + myIdentifier + "}}";
        }
    }

    private final ModuleRegistry myRegistry;
    private SyntaxWraps myWraps;
    private final ArrayList<TopBinding> myBindings =
        new ArrayList<TopBinding>();
    private final ArrayList<FusionValue> myValues =
        new ArrayList<FusionValue>();

    Namespace(ModuleRegistry registry)
    {
        myRegistry = registry;

        SyntaxWrap wrap = new EnvironmentRenameWrap(this);
        SyntaxWraps wraps = SyntaxWraps.make(wrap);

        myWraps = wraps;
    }

    Namespace(ModuleRegistry registry, ModuleInstance language)
    {
        myRegistry = registry;

        SyntaxWrap wrap = new ModuleRenameWrap(language);
        SyntaxWraps wraps = SyntaxWraps.make(wrap);
        wrap = new EnvironmentRenameWrap(this);
        wraps = wraps.addWrap(wrap);

        myWraps = wraps;
    }


    public ModuleRegistry getRegistry()
    {
        return myRegistry;
    }

    @Override
    public Namespace namespace()
    {
        return this;
    }

    @Override
    public int getDepth()
    {
        return 0;
    }

    @Override
    public Object lookup(int rib, int address)
    {
        throw new IllegalStateException("Rib not found");
    }

    ModuleIdentity getModuleId()
    {
        return null;
    }

    Collection<TopBinding> getBindings()
    {
        return Collections.unmodifiableCollection(myBindings);
    }

    //========================================================================

    /**
     * Adds wraps to the syntax object to give it the bindings of this
     * namespace and of required modules.
     */
    SyntaxValue syntaxIntroduce(SyntaxValue source)
    {
        source = source.addWraps(myWraps);
        return source;
    }


    /**
     * @param marks not null.
     *
     * @return null if identifier isn't bound here.
     */
    TopBinding localSubstitute(Binding binding, Set<Integer> marks)
    {
        for (TopBinding b : myBindings)
        {
            Binding resolvedBoundId = b.myIdentifier.resolve();
            if (resolvedBoundId.equals(binding))
            {
                Set<Integer> boundMarks = b.myIdentifier.computeMarks();
                if (marks.equals(boundMarks))
                {
                    return b;
                }
            }
        }

        return null;
    }

    @Override
    public Binding substitute(Binding binding, Set<Integer> marks)
    {
        Binding subst = localSubstitute(binding, marks);
        if (subst == null) subst = binding;
        return subst;
    }


    /**
     * @return null if identifier isn't bound here.
     */
    TopBinding localResolve(SyntaxSymbol identifier)
    {
        Binding resolvedRequestedId = identifier.resolve();
        Set<Integer> marks = identifier.computeMarks();
        return localSubstitute(resolvedRequestedId, marks);
    }


    Binding resolve(String name)
    {
        SyntaxSymbol identifier = SyntaxSymbol.make(name);
        identifier = (SyntaxSymbol) syntaxIntroduce(identifier);
        return identifier.resolve();
    }


    TopBinding newBinding(SyntaxSymbol identifier, int address)
    {
        return new TopBinding(identifier, address);
    }


    /**
     * Creates a binding, but no value, for a name.
     * Used during preparation phase, before evaluating the right-hand side.
     */
    public TopBinding predefine(SyntaxSymbol identifier)
    {
        TopBinding binding = localResolve(identifier);
        if (binding == null)
        {
            int address = myBindings.size();
            binding = newBinding(identifier, address);
            myBindings.add(binding);
        }
        return binding;
    }


    /**
     * Creates or updates a namespace-level binding.
     * Allows rebinding of existing names!
     *
     * @param value must not be null
     */
    void bind(TopBinding binding, FusionValue value)
    {
        set(binding.myAddress, value);

        String inferredName = binding.getName();
        if (inferredName != null) value.inferName(inferredName);
    }


    /**
     * Updates a pre-defined namespace-level variable.
     * Allows rebinding of existing names!
     *
     * @param value must not be null
     */
    @Override
    public void set(int address, Object value)
    {
        int size = myValues.size();
        if (address < size)
        {
            myValues.set(address, (FusionValue) value);
        }
        else // We need to grow myValues. Annoying lack of API to do this.
        {
            myValues.ensureCapacity(myBindings.size()); // Grow all at once
            for (int i = size; i < address; i++)
            {
                myValues.add(null);
            }
            myValues.add((FusionValue) value);
        }
    }


    /**
     * Creates or updates a namespace-level binding.
     * Allows rebinding of existing names!
     *
     * @param value must not be null
     */
    public void bind(String name, FusionValue value)
    {
        SyntaxSymbol identifier = SyntaxSymbol.make(name);
        TopBinding binding = predefine(identifier);
        bind(binding, value);
    }


    void use(ModuleIdentity id)
    {
        ModuleInstance module = myRegistry.lookup(id);
        use(module);
    }

    void use(final ModuleInstance module)
    {
        SyntaxWrap wrap = new ModuleRenameWrap(module);
        myWraps = myWraps.addWrap(wrap);
    }


    @Override
    public FusionValue lookup(Binding binding)
    {
        if (binding instanceof TopBinding)    // else it can't possibly be ours
        {
            return lookup((TopBinding) binding);
        }
        return null;
    }

    public FusionValue lookup(TopBinding binding)
    {
        int address = binding.myAddress;
        if (address < myValues.size())              // for prepare-time lookup
        {
            TopBinding localBinding = myBindings.get(address);
            if (binding.equals(localBinding))
            {
                return myValues.get(address);
            }
        }
        return null;
    }

    @Override
    public Object lookup(int address)
    {
        return myValues.get(address);
    }

    //========================================================================


    /**
     * Creates a new namespace sharing the same {@link ModuleRegistry}.
     */
    Namespace emptyNamespace()
    {
        return new Namespace(myRegistry);
    }


    //========================================================================


    /**
     * A reference to a top-level variable in the lexically-enclosing namespace.
     */
    private static final class CompiledTopVariableReference
        implements CompiledForm
    {
        private final int myAddress;

        CompiledTopVariableReference(int address)
        {
            myAddress = address;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            NamespaceStore ns = store.namespace();
            Object result = ns.lookup(myAddress);
            assert result != null : "No value for namespace address " + myAddress;
            return result;
        }
    }


    private static class CompiledTopDefine
        implements CompiledForm
    {
        private final String       myName;
        private final int          myAddress;
        private final CompiledForm myValueForm;

        CompiledTopDefine(String name, int address, CompiledForm valueForm)
        {
            assert name != null;
            myName      = name;
            myAddress   = address;
            myValueForm = valueForm;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            Object value = eval.eval(store, myValueForm);

            value = processValue(eval, store, value);

            NamespaceStore ns = store.namespace();
            ns.set(myAddress, value);

            if (value instanceof NamedValue)
            {
                ((NamedValue)value).inferName(myName);
            }

            return UNDEF;
        }

        Object processValue(Evaluator eval, Store store, Object value)
            throws FusionException
        {
            return value;
        }
    }


    private static final class CompiledTopDefineSyntax
        extends CompiledTopDefine
    {
        private CompiledTopDefineSyntax(String name, int address,
                                        CompiledForm valueForm)
        {
            super(name, address, valueForm);
        }

        @Override
        Object processValue(Evaluator eval, Store store, Object value)
            throws FusionException
        {
            if (value instanceof Procedure)
            {
                Procedure xformProc = (Procedure) value;
                value = new MacroTransformer(xformProc);
            }
            else if (! (value instanceof KeywordValue))
            {
                String message =
                    "define_syntax value is not a transformer: " +
                    writeToString(value);
                throw new ContractFailure(message);
            }

            return value;
        }
    }
}
