// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.ArrayList;

/**
 *
 */
final class ModuleKeyword
    extends KeywordValue
{
    private final DynamicParameter myCurrentModuleDeclareName;
    private final ModuleNameResolver myModuleNameResolver;

    ModuleKeyword(ModuleNameResolver moduleNameResolver,
                  DynamicParameter currentModuleDeclareName)
    {
        //    "                                                                               |
        super("NAME BODY ...+",
              "Declares a module containing the given body. The NAME must be a symbol.");

        myCurrentModuleDeclareName = currentModuleDeclareName;
        myModuleNameResolver = moduleNameResolver;
    }

    @Override
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp expr)
        throws FusionException
    {
        SyntaxSymbol name = requiredSymbol("module name", 1, expr);
        String declaredName = name.stringValue();
        // TODO check null/empty

        SyntaxValue initialBindingsStx = requiredForm("initial module path", 2, expr);

        Namespace ns = env.namespace();
        Namespace moduleNamespace = eval.newEmptyNamespace(ns);

        try
        {
            ModuleIdentity initialBindingsId =
                myModuleNameResolver.resolve(eval, initialBindingsStx);
            moduleNamespace.use(initialBindingsId);
        }
        catch (FusionException e)
        {
            String message =
                "Error installing initial bindings: " + e.getMessage();
            throw new FusionException(message, e);
        }

        ArrayList<SyntaxSexp> provideForms = new ArrayList<SyntaxSexp>();

        for (int i = 3; i < expr.size(); i++)
        {
            SyntaxValue form = expr.get(i);
            SyntaxSexp provide = formIsProvide(form);
            if (provide != null)
            {
                provideForms.add(provide);
            }
            else
            {
                eval.eval(moduleNamespace, form);
            }
        }

        String[] providedNames =
            processProvides(provideForms, moduleNamespace);

        ModuleIdentity id = determineIdentity(eval, declaredName);
        ModuleInstance module =
            new ModuleInstance(id, moduleNamespace, providedNames);
        moduleNamespace.getRegistry().register(module);
        return module;
    }

    private ModuleIdentity determineIdentity(Evaluator eval,
                                             String declaredName)
        throws FusionException
    {
        ModuleIdentity id;
        String current = myCurrentModuleDeclareName.asString(eval);
        if (current != null)
        {
            id = ModuleIdentity.intern(current);
        }
        else
        {
            id = ModuleIdentity.intern(declaredName);
        }
        return id;
    }

    private SyntaxSexp formIsProvide(SyntaxValue form)
    {
        if (form.getType() == SyntaxValue.Type.SEXP)
        {
            SyntaxSexp sexp = (SyntaxSexp) form;
            if (sexp.size() != 0)
            {
                SyntaxValue first = sexp.get(0);
                if (first.getType() == SyntaxValue.Type.SYMBOL
                    && "provide".equals(((SyntaxSymbol) first).stringValue()))
                {
                    return sexp;
                }
            }
        }
        return null;
    }

    /**
     * @param provideForms
     * @param moduleNamespace
     * @return
     */
    private String[] processProvides(ArrayList<SyntaxSexp> provideForms,
                                     Namespace moduleNamespace)
        throws SyntaxFailure
    {
        ArrayList<String> names = new ArrayList<String>();

        for (SyntaxSexp form : provideForms)
        {
            int size = form.size();
            SyntaxChecker check = new SyntaxChecker("provide", form);
            for (int i = 1; i < size; i++)
            {
                String name =
                    check.requiredNonEmptySymbol("bound identifier", i);

                if (moduleNamespace.lookup(name) == null)
                {
                    throw check.failure("name is not bound: " + name);
                }

                names.add(name);
            }
        }

        return names.toArray(new String[names.size()]);
    }

}
