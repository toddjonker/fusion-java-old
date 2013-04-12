// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.ModuleNamespace.ModuleBinding;
import com.amazon.fusion.Namespace.NsBinding;
import java.util.Iterator;
import java.util.Set;

/**
 * Syntax wrap that adds all bindings provided by a "language" module.
 * We use special bindings so that we can distinguish from those that are
 * introduced via regular imports, since these bindings can be shadowed by
 * such imports and by module-level definitions.
 */
final class LanguageWrap
    extends SyntaxWrap
{
    static final class LanguageBinding
        extends NsBinding
        implements Binding
    {
        private final ModuleBinding myBinding;

        private LanguageBinding(ModuleBinding original)
        {
            super(original.getIdentifier(), original.myAddress);
            myBinding = original;
            assert myBinding == myBinding.originalBinding();
        }

        @Override
        public Binding originalBinding()
        {
            return myBinding;
        }

        @Override
        public boolean sameTarget(Binding other)
        {
            return myBinding == other.originalBinding();
        }

        @Override
        public Object lookup(Environment store)
        {
            return myBinding.lookup(store);
        }

        @Override
        public CompiledForm compileReference(Evaluator eval, Environment env)
            throws FusionException
        {
            return myBinding.compileReference(eval, env);
        }

        @Override
        public CompiledForm compileSet(Evaluator eval, Environment env,
                                       CompiledForm valueForm)
            throws FusionException
        {
            return myBinding.compileSet(eval, env, valueForm);
        }

        @Override
        public boolean equals(Object other)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        public String toString()
        {
            return "{{{LanguageBinding " + myBinding.myModuleId +
                   ' ' + getName() + "}}}";
        }
    }


    private final ModuleInstance myModule;

    LanguageWrap(ModuleInstance module)
    {
        assert module != null;
        myModule = module;
    }


    @Override
    Binding resolve(String name,
                    Iterator<SyntaxWrap> moreWraps,
                    Set<Integer> returnMarks)
    {
        assert ! moreWraps.hasNext();
        // If that's not always true we must collect marks from moreWraps.
        // and perhaps look up name there too?

        ModuleBinding local = myModule.resolveProvidedName(name);
        if (local != null)
        {
            return new LanguageBinding(local);
        }

        return new FreeBinding(name);
    }


    @Override
    public String toString()
    {
        String id = myModule.getIdentity().internString();
        return "{{{Language wrap for " + id + "}}}";
    }
}
