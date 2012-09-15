// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Set;

class Namespace
    implements Environment, RuntimeNamespace
{
    static class NsBinding implements Binding
    {
        private final SyntaxSymbol myIdentifier;
        final int myAddress;

        NsBinding(SyntaxSymbol identifier, int address)
        {
            assert identifier.resolve() instanceof FreeBinding;
            myIdentifier = identifier;
            myAddress = address;
        }

        final SyntaxSymbol getIdentifier()
        {
            return myIdentifier;
        }

        final String getName()
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
        public CompiledForm compile(Evaluator eval, Environment env)
            throws FusionException
        {
            return new CompiledTopVariable(myAddress);
        }

        @Override
        public boolean equals(Object other)
        {
            return this == other;
        }

        @Override
        public String toString()
        {
            return "{{NsBinding " + myIdentifier + "}}";
        }
    }

    private final ModuleRegistry myRegistry;
    private SyntaxWraps myWraps;
    private final ArrayList<NsBinding> myBindings =
        new ArrayList<NsBinding>();
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

    ModuleIdentity getModuleId()
    {
        return null;
    }

    @Override
    public RuntimeNamespace runtimeNamespace()
    {
        return this;
    }

    Collection<NsBinding> getBindings()
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
    NsBinding localSubstitute(Binding binding, Set<Integer> marks)
    {
        for (NsBinding b : myBindings)
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
    NsBinding localResolve(SyntaxSymbol identifier)
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


    NsBinding newBinding(SyntaxSymbol identifier, int address)
    {
        return new NsBinding(identifier, address);
    }


    /**
     * Creates a binding, but no value, for a name.
     * Used during preparation phase, before evaluating the right-hand side.
     */
    public NsBinding predefine(SyntaxSymbol identifier)
    {
        NsBinding binding = localResolve(identifier);
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
    void bind(NsBinding binding, FusionValue value)
    {
        int address = binding.myAddress;
        int size = myValues.size();
        if (address < size)
        {
            myValues.set(address, value);
        }
        else // We need to grow myValues. Annoying lack of API to do this.
        {
            myValues.ensureCapacity(myBindings.size()); // Grow all at once
            for (int i = size; i < address; i++)
            {
                myValues.add(null);
            }
            myValues.add(value);
        }

        String inferredName = binding.myIdentifier.stringValue();
        if (inferredName != null) value.inferName(inferredName);
    }


    /**
     * Creates or updates a namespace-level binding.
     * Allows rebinding of existing names!
     *
     * @param value must not be null
     */
    @Override
    public void bindPredefined(NsBinding binding, Object value)
    {
        int address = binding.myAddress;
        if (address < myBindings.size())
        {
            assert binding == myBindings.get(address);
        }
        else
        {
            assert address == myBindings.size();
            myBindings.add(binding);
        }

        FusionValue fv = (FusionValue) value;
        bind(binding, fv);  // TODO this is inefficient
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
        NsBinding binding = predefine(identifier);
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
        if (binding instanceof NsBinding)    // else it can't possibly be ours
        {
            return lookup((NsBinding) binding);
        }
        return null;
    }

    public FusionValue lookup(NsBinding binding)
    {
        int address = binding.myAddress;
        if (address < myValues.size())              // for prepare-time lookup
        {
            NsBinding localBinding = myBindings.get(address);
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
     * A reference to a top-level binding in the lexically-enclosing namespace.
     */
    private static final class CompiledTopVariable
        implements CompiledForm
    {
        private final int myAddress;

        CompiledTopVariable(int address)
        {
            myAddress = address;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            RuntimeNamespace ns = store.runtimeNamespace();
            Object result = ns.lookup(myAddress);
            assert result != null : "No value for " + myAddress;
            return result;
        }
    }
}
