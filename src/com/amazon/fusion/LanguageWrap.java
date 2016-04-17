// Copyright (c) 2013-2016 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.FusionSymbol.BaseSymbol;
import com.amazon.fusion.ModuleNamespace.ModuleBinding;
import java.util.Iterator;
import java.util.Set;

/**
 * Syntax wrap that adds all bindings provided by a "language" module.
 * We use special bindings so that we can distinguish from those that are
 * introduced via regular imports, since these bindings can be shadowed by
 * such imports and by module-level definitions.
 * <p>
 * In addition, a language wrap doesn't look for bindings in pre-existing wraps
 * since no other bindings should "leak" through. This is meaningful for
 * modules declared in scripts and for nested modules, but may need to be
 * changed in order to implement {@code module*}, which can be used to view the
 * enclosing module's binding.
 */
final class LanguageWrap
    extends SyntaxWrap
{
    /**
     * Denotes a binding imported into a module via the language declared at
     * its creation.
     */
    static final class LanguageBinding
        extends Binding
    {
        private final ModuleBinding myTarget;

        private LanguageBinding(ModuleBinding target)
        {
            assert target.target() == target;
            myTarget = target;
        }

        @Override
        public final BaseSymbol getName()
        {
            return myTarget.getName();
        }

        @Override
        public Binding target()
        {
            return myTarget;
        }

        @Override
        public Object lookup(Namespace ns)
        {
            return myTarget.lookup(ns);
        }

        @Override
        public CompiledForm compileReference(Evaluator eval, Environment env)
            throws FusionException
        {
            return myTarget.compileReference(eval, env);
        }

        @Override
        public CompiledForm compileTopReference(Evaluator eval,
                                                Environment env,
                                                SyntaxSymbol id)
            throws FusionException
        {
            String message =
                "#%top not implemented for language binding: " + this;
            throw new SyntaxException("#%top", message, id);
        }

        @Override
        public CompiledForm compileSet(Evaluator eval, Environment env,
                                       CompiledForm valueForm)
            throws FusionException
        {
            return myTarget.compileSet(eval, env, valueForm);
        }

        @Override
        public boolean equals(Object other)
        {
            throw new UnsupportedOperationException();
        }

        @Override
        public String toString()
        {
            return "{{{LanguageBinding " + myTarget.myModuleId.absolutePath()
                 + ' ' + getName() + "}}}";
        }
    }


    private final ModuleInstance myModule;

    LanguageWrap(ModuleInstance module)
    {
        assert module != null;
        myModule = module;
    }


    @Override
    Binding resolve(BaseSymbol name,
                    Iterator<SyntaxWrap> moreWraps,
                    Set<MarkWrap> returnMarks)
    {
        if (moreWraps.hasNext())
        {
            SyntaxWrap nextWrap = moreWraps.next();
            // Prior bindings never "leak through" a language, so we ignore
            // this binding.  We still want to collect the marks though.
            nextWrap.resolve(name, moreWraps, returnMarks);
        }

        // Language-provided bindings never have marks!
        ModuleBinding local = myModule.resolveProvidedName(name);
        if (local != null)
        {
            return new LanguageBinding(local);
        }

        return null;
    }


    @Override
    Iterator<SyntaxWrap> iterator()
    {
        return null;
    }


    @Override
    public String toString()
    {
        String id = myModule.getIdentity().absolutePath();
        return "{{{Language wrap for " + id + "}}}";
    }
}
