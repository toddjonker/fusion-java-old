// Copyright (c) 2012-2016 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionIo.safeWriteToString;
import static com.amazon.fusion.FusionVoid.voidValue;
import com.amazon.fusion.FusionSymbol.BaseSymbol;
import com.amazon.fusion.ModuleNamespace.ModuleDefinedBinding;
import com.amazon.fusion.ModuleNamespace.ProvidedBinding;
import com.amazon.fusion.TopLevelNamespace.TopLevelBinding;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

/**
 * Expand- and compile-time environment for all top-level sequences, and
 * eval-time store for non-module top levels.
 * <p>
 * Since top-levels and modules have different behavior around imports and
 * defines, that responsibility is delegated to subclasses.
 */
abstract class Namespace
    implements Environment, NamespaceStore
{
    /**
     * Denotes a namespace-level binding, either top-level or module-level.
     *
     * @see TopLevelBinding
     * @see ModuleDefinedBinding
     */
    abstract static class NsDefinedBinding
        extends Binding
    {
        private final SyntaxSymbol myIdentifier;
        final int myAddress;

        NsDefinedBinding(SyntaxSymbol identifier, int address)
        {
            myIdentifier = identifier;
            myAddress = address;
        }

        @Override
        final BaseSymbol getName()
        {
            return myIdentifier.getName();
        }

        final SyntaxSymbol getIdentifier()
        {
            return myIdentifier;
        }

        final CompiledForm compileLocalTopReference(Evaluator   eval,
                                                    Environment env)
            throws FusionException
        {
            // TODO This fails when a macro references a prior local defn
            // since the defn isn't installed yet.  I think the code is bad
            // and mixes phases of macro processing.
//          assert (env.namespace().ownsBinding(this));
            return new CompiledTopVariableReference(myAddress);
        }

        @Override
        final CompiledForm compileSet(Evaluator eval, Environment env,
                                      CompiledForm valueForm)
            throws FusionException
        {
            throw new IllegalStateException("Mutation should have been rejected");
        }

        CompiledForm compileDefineSyntax(Evaluator eval,
                                         Environment env,
                                         CompiledForm valueForm)
        {
            String name = getName().stringValue();
            return new CompiledTopDefineSyntax(name, myAddress, valueForm);
        }

        @Override
        public boolean equals(Object other)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        public final int hashCode()
        {
            return System.identityHashCode(this);
        }

        @Override
        public abstract String toString(); // Force subclasses to implement
    }


    /**
     * A binding added to a namepace via {@code require} or a language
     * declaration.
     */
    abstract static class RequiredBinding
        extends Binding
    {
        final SyntaxSymbol    myIdentifier;
        final ProvidedBinding myTarget;

        RequiredBinding(SyntaxSymbol identifier, ProvidedBinding target)
        {
            myIdentifier = identifier;
            myTarget = target;
        }

        @Override
        final BaseSymbol getName()
        {
            return myIdentifier.getName();
        }

        final SyntaxSymbol getIdentifier()
        {
            return myIdentifier;
        }

        @Override
        final ModuleDefinedBinding target()
        {
            return myTarget.target();
        }

        @Override
        final Object lookup(Namespace ns)
        {
            return myTarget.lookup(ns);
        }

        @Override
        final CompiledForm compileReference(Evaluator eval, Environment env)
            throws FusionException
        {
            return myTarget.compileReference(eval, env);
        }

        @Override
        final CompiledForm compileSet(Evaluator eval, Environment env,
                                      CompiledForm valueForm)
            throws FusionException
        {
            // This isn't currently reachable, but it's an easy safeguard.
            String message = "Mutation of imported binding is not allowed";
            throw new ContractException(message);
        }
    }


    static class RequiredBindingMap<B extends RequiredBinding>
    {
        /**
         * Maps each imported name to the bindings associated with it.
         * There may be multiple variants since the same name may occur with
         * different marks.
         */
        private final Map<BaseSymbol, RequiredBinding[]> myBindings =
            new IdentityHashMap<>();

        void put(SyntaxSymbol localId, B binding)
            throws AmbiguousBindingFailure
        {
            BaseSymbol name = localId.getName();

            RequiredBinding[] variants = myBindings.get(name);
            if (variants == null)
            {
                variants = new RequiredBinding[] { binding };
            }
            else
            {
                // Do we already have a required binding that matches?
                boolean matched = false;

                int len = variants.length;
                for (int i = 0; i < len; i++)
                {
                    @SuppressWarnings("unchecked")
                    B current = (B) variants[i];

                    if (localId.freeIdentifierEqual(current.getIdentifier()))
                    {
                        checkReplacement(current, binding);
                        variants[i] = binding;
                        matched = true;
                        break;
                    }
                }

                if (! matched)
                {
                    variants = Arrays.copyOf(variants, len + 1);
                    variants[len] = binding;
                }
            }
            myBindings.put(name, variants);
        }

        /** Throws an exception if we can't replace a binding. */
        void checkReplacement(B current, B replacement)
            throws AmbiguousBindingFailure
        {
        }

        @SuppressWarnings("unchecked")
        B get(BaseSymbol name, Set<MarkWrap> marks)
        {
            RequiredBinding[] variants = myBindings.get(name);
            if (variants != null)
            {
                for (RequiredBinding b : variants)
                {
                    SyntaxSymbol id = b.getIdentifier();
                    assert id.getName() == name;
                    if (id.resolvesFree(name, marks))
                    {
                        return (B) b;
                    }
                }
            }
            return null;
        }
    }


    private final ModuleRegistry myRegistry;
    private final ModuleIdentity myModuleId;

    /**
     * Assigns required modules to integer addresses, for use in compiled
     * forms.
     */
    private final HashMap<ModuleIdentity,Integer> myRequiredModules =
        new HashMap<>();
    private final ArrayList<ModuleStore> myRequiredModuleStores =
        new ArrayList<>();

    private final SyntaxWraps          myWraps;
    private final ArrayList<NsDefinedBinding> myDefinedBindings = new ArrayList<>();
    private final ArrayList<Object>    myValues   = new ArrayList<>();
    private ArrayList<BindingDoc> myBindingDocs;


    /**
     * @param registry must not be null.
     * @param id must not be null.
     * @param wraps generates the {@link SyntaxWraps} for this namespace, given
     *   a reference to {@code this}.
     */
    Namespace(ModuleRegistry                   registry,
              ModuleIdentity                   id,
              Function<Namespace, SyntaxWraps> wraps)
    {
        myRegistry = registry;
        myModuleId = id;
        myWraps    = wraps.apply(this);
    }

    @Override
    public final ModuleRegistry getRegistry()
    {
        return myRegistry;
    }

    @Override
    public final Namespace namespace()
    {
        return this;
    }

    @Override
    public final int getDepth()
    {
        return 0;
    }

    @Override
    public final Object lookup(int rib, int address)
    {
        String message = "Rib not found: " + rib + ',' + address;
        throw new IllegalStateException(message);
    }

    @Override
    public final void set(int rib, int address, Object value)
    {
        String message = "Rib not found: " + rib + ',' + address;
        throw new IllegalStateException(message);
    }

    /**
     * Gets the identity of the module associated with this namespace.
     * For non-module namespaces, this returns a synthetic identity that's
     * unique amongst all namespaces.
     *
     * @return not null.
     */
    final ModuleIdentity getModuleId()
    {
        return myModuleId;
    }

    /**
     * How many namspace-level definitions do we have?
     * This doesn't count imports.
     */
    final int definitionCount()
    {
        return myDefinedBindings.size();
    }

    /**
     * Collects the bindings defined in this namespace; does not include imported
     * bindings.
     *
     * @return not null.
     */
    final Collection<NsDefinedBinding> getDefinedBindings()
    {
        return Collections.unmodifiableCollection(myDefinedBindings);
    }

    //========================================================================

    /**
     * Adds wraps to the syntax object to give it the bindings of this
     * namespace and of required modules.
     */
    final SyntaxValue syntaxIntroduce(SyntaxValue source)
        throws FusionException
    {
        // TODO there's a case where we are applying the same wraps that are
        // already on the source.  This happens when expand-ing (and maybe when
        // eval-ing at top-level source that's from that same context.
        source = source.addWraps(myWraps);
        return source;
    }


    /**
     * @param marks not null.
     *
     * @return null if identifier isn't bound here.
     */
    final NsDefinedBinding substituteDefinition(Binding       binding,
                                                Set<MarkWrap> marks)
    {
        for (NsDefinedBinding b : myDefinedBindings)
        {
            if (b.myIdentifier.resolvesBound(binding, marks))
            {
                return b;
            }
        }
        return null;
    }

    @Override
    public final Binding substitute(Binding binding, Set<MarkWrap> marks)
    {
        Binding subst = substituteDefinition(binding, marks);
        if (subst == null) subst = binding;
        return subst;
    }

    @Override
    public final NsDefinedBinding substituteFree(BaseSymbol    name,
                                                 Set<MarkWrap> marks)
    {
        for (NsDefinedBinding b : myDefinedBindings)
        {
            if (b.myIdentifier.resolvesFree(name, marks))
            {
                return b;
            }
        }
        return null;
    }


    /**
     * Resolves an identifier to a namespace-level definition (not an import).
     *
     * @return null if identifier isn't defined here.
     */
    final NsDefinedBinding resolveDefinition(SyntaxSymbol identifier)
    {
        Binding resolvedRequestedId = identifier.resolve();
        Set<MarkWrap> marks = identifier.computeMarks();
        if (resolvedRequestedId instanceof FreeBinding)
        {
            return substituteFree(identifier.getName(), marks);
        }
        return substituteDefinition(resolvedRequestedId, marks);
    }


    /**
     * @param name must be non-empty.
     *
     * @return null is equivalent to a {@link FreeBinding}.
     */
    final Binding resolve(BaseSymbol name)
    {
        return myWraps.resolve(name);
    }

    /**
     * @param name must be non-empty.
     *
     * @return null is equivalent to a {@link FreeBinding}.
     */
    final Binding resolve(String name)
    {
        BaseSymbol symbol = FusionSymbol.makeSymbol(null, name);
        return resolve(symbol);
    }


    abstract NsDefinedBinding newDefinedBinding(SyntaxSymbol identifier, int address);


    final NsDefinedBinding addDefinedBinding(SyntaxSymbol identifier)
        throws FusionException
    {
        int address = myDefinedBindings.size();
        NsDefinedBinding binding = newDefinedBinding(identifier, address);
        myDefinedBindings.add(binding);
        return binding;
    }


    /**
     * Creates a binding, but no value, for a name.
     * Used during expansion phase, before evaluating the right-hand side.
     *
     * @return a copy of the identifier that has the new binding attached.
     */
    abstract SyntaxSymbol predefine(SyntaxSymbol identifier,
                                    SyntaxValue formForErrors)
        throws FusionException;


    /**
     * Creates or updates a namespace-level binding.
     * Allows rebinding of existing names!
     *
     * @param value must not be null
     */
    final void bind(NsDefinedBinding binding, Object value)
    {
        set(binding.myAddress, value);

        if (value instanceof NamedValue)
        {
            String inferredName = binding.getName().stringValue();
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
        else // We need to grow the list. Annoying lack of API to do this.
        {
            list.ensureCapacity(myDefinedBindings.size()); // Grow all at once
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
    public final void set(int address, Object value)
    {
        set(myValues, address, value);
    }


    /**
     * Creates or updates a namespace-level binding.
     * Allows rebinding of existing names!
     *
     * @param value must not be null
     *
     * @throws IllegalArgumentException if the name is null or empty.
     */
    public final void bind(String name, Object value)
        throws FusionException
    {
        if (name == null || name.length() == 0)
        {
            String message = "bound name must be non-null and non-empty";
            throw new IllegalArgumentException(message);
        }

        // WARNING: We pass null evaluator because we know its not used.
        //          That is NOT SUPPORTED for user code!
        SyntaxSymbol identifier = SyntaxSymbol.make(null, name);

        identifier = predefine(identifier, null);
        NsDefinedBinding binding = (NsDefinedBinding) identifier.getBinding();
        bind(binding, value);
    }


    //========================================================================
    //  Require

    /**
     * Denotes a new binding created by {@code require}, possibly renaming the
     * exported symbol along the way.
     */
    static final class RequireRenameMapping
    {
        final SyntaxSymbol myLocalIdentifier;
        final BaseSymbol   myExportedIdentifier;

        RequireRenameMapping(SyntaxSymbol localIdentifier,
                             BaseSymbol exportedIdentifier)
        {
            myLocalIdentifier = localIdentifier;
            myExportedIdentifier = exportedIdentifier;
        }

        @Override
        public String toString()
        {
            return "RequireRenameMapping::{local:" + myLocalIdentifier
                + ",exported:" + myExportedIdentifier + '}';
        }
    }


    /**
     * @param modulePath is an absolute or relative module path.
     */
    final void require(Evaluator eval, String modulePath)
        throws FusionException
    {
        ModuleNameResolver resolver =
            eval.getGlobalState().myModuleNameResolver;
        ModuleIdentity id =
            resolver.resolveModulePath(eval,
                                       getModuleId(),
                                       modulePath,
                                       true /* load */,
                                       null /* stxForErrors */);
        require(eval, id);
    }

    /**
     * Imports all exported bindings from a module.
     * This is used by {@code (require module_path)}.
     */
    final void require(Evaluator eval, ModuleIdentity id)
        throws FusionException
    {
        ModuleInstance module = myRegistry.instantiate(eval, id);
        require(eval, module);
    }

    void require(Evaluator eval, ModuleInstance module)
        throws FusionException
    {
        for (ProvidedBinding provided : module.providedBindings())
        {
            // TODO FUSION-117 Not sure this is the right lexical context.
            // The identifier is free, but references will have context
            // including the language.
            SyntaxSymbol id = SyntaxSymbol.make(eval, null, provided.getName());
            installRequiredBinding(id, provided);
        }
    }


    final void require(Evaluator eval, ModuleIdentity id,
                       Iterator<RequireRenameMapping> mappings)
        throws FusionException
    {
        ModuleInstance module = myRegistry.instantiate(eval, id);
        require(eval, module, mappings);
    }

    void require(Evaluator eval, ModuleInstance module,
                 Iterator<RequireRenameMapping> mappings)
        throws FusionException
    {
        while (mappings.hasNext())
        {
            RequireRenameMapping mapping = mappings.next();
            BaseSymbol exportedId = mapping.myExportedIdentifier;
            ProvidedBinding provided = module.resolveProvidedName(exportedId);
            installRequiredBinding(mapping.myLocalIdentifier, provided);
        }
    }


    abstract void installRequiredBinding(SyntaxSymbol    localId,
                                         ProvidedBinding target)
        throws AmbiguousBindingFailure;


    //========================================================================


    final boolean ownsBinding(NsDefinedBinding binding)
    {
        int address = binding.myAddress;
        return (address < myDefinedBindings.size()
                && binding == myDefinedBindings.get(address));
    }

    final boolean ownsBinding(Binding binding)
    {
        if (binding instanceof NsDefinedBinding)
        {
            return ownsBinding((NsDefinedBinding) binding);
        }
        return false;
    }



    abstract CompiledForm compileDefine(Evaluator eval,
                                        FreeBinding binding,
                                        SyntaxSymbol id,
                                        CompiledForm valueForm)
        throws FusionException;

    abstract CompiledForm compileDefine(Evaluator eval,
                                        TopLevelBinding binding,
                                        SyntaxSymbol id,
                                        CompiledForm valueForm)
        throws FusionException;

    abstract CompiledForm compileDefine(Evaluator eval,
                                        ModuleDefinedBinding binding,
                                        SyntaxSymbol id,
                                        CompiledForm valueForm)
        throws FusionException;


    /**
     * Compile a free variable reference.  These are allowed at top-level but
     * not within a module.
     */
    abstract CompiledForm compileFreeTopReference(SyntaxSymbol identifier)
        throws FusionException;


    /**
     * Looks for a definition's value in this namespace, ignoring imports.
     *
     * @return the binding's value, or null if there is none.
     */
    final Object lookupDefinition(NsDefinedBinding binding)
    {
        int address = binding.myAddress;
        if (address < myValues.size())              // for prepare-time lookup
        {
            NsDefinedBinding localBinding = myDefinedBindings.get(address);
            if (binding == localBinding)
            {
                return myValues.get(address);
            }
        }
        return null;
    }


    /**
     * Looks for a binding's value in this namespace, finding both definitions
     * and imports.
     *
     * @return the binding's value, or null if there is none.
     */
    final Object lookup(Binding binding)
    {
        return binding.lookup(this);
    }


    /**
     * Looks for a binding's value in this namespace, finding both definitions
     * and imports.
     *
     * @return the binding's value, or null if there is none.
     */
    final Object lookup(String name)
    {
        Binding b = resolve(name);
        if (b == null)
        {
            return b;
        }
        else
        {
            return lookup(b);
        }
    }


    @Override
    public final Object lookup(int address)
    {
        return myValues.get(address);
    }

    @Override
    public final Object lookupImport(int moduleAddress, int bindingAddress)
    {
        ModuleStore store = myRequiredModuleStores.get(moduleAddress);
        return store.lookup(bindingAddress);
    }

    final Object[] extractValues()
    {
        return myValues.toArray();
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
    final synchronized int requiredModuleAddress(ModuleIdentity moduleId)
    {
        Integer id = myRequiredModules.get(moduleId);
        if (id == null)
        {
            ModuleInstance module = myRegistry.lookup(moduleId);

            id = myRequiredModules.size();
            myRequiredModules.put(moduleId, id);
            myRequiredModuleStores.add(module.getNamespace());
        }
        return id;
    }

    final synchronized ModuleIdentity[] requiredModuleIds()
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

    @Override
    public final ModuleStore lookupRequiredModule(int moduleAddress)
    {
        return myRequiredModuleStores.get(moduleAddress);
    }


    //========================================================================
    // Documentation


    final void setDoc(String name, BindingDoc.Kind kind, String doc)
    {
        BindingDoc bDoc = new BindingDoc(name, kind,
                                         null, // usage
                                         doc);
        setDoc(name, bDoc);
    }

    final void setDoc(String name, BindingDoc doc)
    {
        NsDefinedBinding binding = (NsDefinedBinding) resolve(name);
        setDoc(binding.myAddress, doc);
    }

    public void setDoc(int address, BindingDoc doc)
    {
        if (myBindingDocs == null)
        {
            myBindingDocs = new ArrayList<BindingDoc>();
        }
        set(myBindingDocs, address, doc);
    }

    final BindingDoc document(int address)
    {
        if (myBindingDocs != null && address < myBindingDocs.size())
        {
            return myBindingDocs.get(address);
        }
        return null;
    }

    /**
     * @return may be shorter than the number of provided variables.
     */
    final BindingDoc[] extractBindingDocs()
    {
        if (myBindingDocs == null) return BindingDoc.EMPTY_ARRAY;

        BindingDoc[] docs = myBindingDocs.toArray(BindingDoc.EMPTY_ARRAY);
        myBindingDocs = null;
        return docs;
    }


    //========================================================================


    /**
     * A reference to a top-level variable in the lexically-enclosing namespace.
     */
    static final class CompiledTopVariableReference
        implements CompiledForm
    {
        final int myAddress;

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


    static class CompiledTopDefine
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
        public final Object doEval(Evaluator eval, Store store)
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
                value = new MacroForm(xformProc);
            }
            else if (! (value instanceof SyntacticForm))
            {
                String message =
                    "define_syntax value is not a transformer: " +
                    safeWriteToString(eval, value);
                throw new ContractException(message);
            }

            return value;
        }
    }
}
