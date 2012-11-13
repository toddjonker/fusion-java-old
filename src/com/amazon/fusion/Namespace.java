// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionValue.writeToString;
import static com.amazon.fusion.FusionVoid.voidValue;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
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
        public Object lookup(Environment env)
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

        @Override
        public CompiledForm compileSet(Evaluator eval, Environment env,
                                       CompiledForm valueForm)
            throws FusionException
        {
            String message =
                "Mutation of top-level variables is not supported";
            throw new ContractFailure(message);
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

    /**
     * Assigns required modules to integer addresses, for use in compiled
     * forms.
     */
    private final HashMap<ModuleIdentity,Integer> myRequiredModules =
        new HashMap<ModuleIdentity,Integer>();
    private final ArrayList<ModuleStore> myRequiredModuleStores =
        new ArrayList<ModuleStore>();

    private SyntaxWraps myWraps;
    private final ArrayList<TopBinding> myBindings =
        new ArrayList<TopBinding>();
    private final ArrayList<Object> myValues =
        new ArrayList<Object>();
    private final ArrayList<BindingDoc> myBindingDocs =
        new ArrayList<BindingDoc>();

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


    @Override
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

    @Override
    public void set(int rib, int address, Object value)
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
    void bind(TopBinding binding, Object value)
    {
        set(binding.myAddress, value);

        if (value instanceof NamedValue)
        {
            String inferredName = binding.getName();
            if (inferredName != null)
            {
                ((NamedValue)value).inferName(inferredName);
            }
        }
    }


    private <T> void set(ArrayList<T> list, int address, T value)
    {
        int size = list.size();
        if (address < size)
        {
            list.set(address, value);
        }
        else // We need to grow myValues. Annoying lack of API to do this.
        {
            list.ensureCapacity(myBindings.size()); // Grow all at once
            for (int i = size; i < address; i++)
            {
                list.add(null);
            }
            list.add(value);
        }
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
        set(myValues, address, value);
    }


    /**
     * Creates or updates a namespace-level binding.
     * Allows rebinding of existing names!
     *
     * @param value must not be null
     */
    public void bind(String name, Object value)
    {
        SyntaxSymbol identifier = SyntaxSymbol.make(name);
        TopBinding binding = predefine(identifier);
        bind(binding, value);
    }


    void use(Evaluator eval, String modulePath)
        throws FusionException
    {
        UseForm useForm = eval.getGlobalState().myUseForm;
        SyntaxValue baseRef = SyntaxSymbol.make(modulePath);
        useForm.use(eval, this, baseRef);
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
    public Object lookup(Binding binding)
    {
        if (binding instanceof TopBinding)    // else it can't possibly be ours
        {
            return lookup((TopBinding) binding);
        }
        return null;
    }

    public Object lookup(TopBinding binding)
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

    Object lookup(String name)
    {
        Binding b = resolve(name);
        return lookup(b);
    }


    @Override
    public Object lookup(int address)
    {
        return myValues.get(address);
    }

    @Override
    public Object lookupImport(int moduleAddress, int bindingAddress)
    {
        ModuleStore store = myRequiredModuleStores.get(moduleAddress);
        return store.lookup(bindingAddress);
    }

    Object[] extractValues()
    {
        Object[] values = new Object[myValues.size()];
        myValues.toArray(values);
        return values;
    }


    /**
     * Translates a required module identity into an integer address for use
     * by compiled forms.  Note that some required modules may not be
     * explicitly declared in the source of the module, since they may come in
     * via macro expansion.
     * <p>
     * Building this list is delayed to compile-time to avoid compiling
     * addresses for modules that are declared but never used.
     * This may be a useless optimization.
     * <p>
     * @return a zero-based address for the module, valid only within this
     * namespace (or its compiled form).
     */
    synchronized int requiredModuleAddress(ModuleIdentity moduleId)
    {
        Integer id = myRequiredModules.get(moduleId);
        if (id == null)
        {
            id = myRequiredModules.size();
            myRequiredModules.put(moduleId, id);

            ModuleInstance module = myRegistry.lookup(moduleId);
            myRequiredModuleStores.add(module.getNamespace());
        }
        return id;
    }

    synchronized ModuleIdentity[] requiredModuleIds()
    {
        ModuleIdentity[] ids = new ModuleIdentity[myRequiredModules.size()];
        for (Map.Entry<ModuleIdentity, Integer> entry
                : myRequiredModules.entrySet())
        {
            int address = entry.getValue();
            ids[address] = entry.getKey();
        }
        return ids;
    }


    //========================================================================
    // Documentation


    public void setDoc(int address, BindingDoc doc)
    {
        set(myBindingDocs, address, doc);
    }


    /**
     * @return may be shorter than the number of top-level variables.
     */
    BindingDoc[] bindingDocs()
    {
        BindingDoc[] docs = new BindingDoc[myBindingDocs.size()];
        return myBindingDocs.toArray(docs);
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

            return voidValue(eval);
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
            else if (! (value instanceof SyntacticForm))
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
