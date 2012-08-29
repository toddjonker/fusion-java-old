// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static java.lang.Boolean.TRUE;
import com.amazon.fusion.Environment.Binding;
import java.util.ArrayList;
import java.util.IdentityHashMap;

/**
 * @see <a href="http://docs.racket-lang.org/reference/module.html">Racket
 * modules</a>
 */
final class ModuleKeyword
    extends KeywordValue
{
    private final DynamicParameter myCurrentModuleDeclareName;
    private final ModuleNameResolver myModuleNameResolver;
    private final Binding myDefineBinding;
    private final Binding myDefineSyntaxBinding;
    private final Binding myUseSyntaxBinding;
    private final IdentityHashMap<Binding, Object> myPartialExpansionStops;

    ModuleKeyword(ModuleNameResolver moduleNameResolver,
                  DynamicParameter currentModuleDeclareName,
                  Namespace kernelNamespace)
    {
        //    "                                                                               |
        super("NAME BODY ...+",
              "Declares a module containing the given body. The NAME must be a symbol.");

        myCurrentModuleDeclareName = currentModuleDeclareName;
        myModuleNameResolver = moduleNameResolver;

        myPartialExpansionStops = new IdentityHashMap<Binding, Object>();
        myDefineBinding       = stopBinding(kernelNamespace, "define");
        myDefineSyntaxBinding = stopBinding(kernelNamespace, "define_syntax");
        myUseSyntaxBinding    = stopBinding(kernelNamespace, "use");
    }

    private Binding stopBinding(Namespace kernel, String name)
    {
        Binding b = kernel.resolve(name);
        assert b != null;
        myPartialExpansionStops.put(b, TRUE);
        return b;
    }

    @Override
    SyntaxValue prepare(Evaluator eval,
                        Environment envOutsideModule,
                        SyntaxSexp source)
        throws SyntaxFailure
    {
        SyntaxChecker check = new SyntaxChecker(getInferredName(), source);

        SyntaxSymbol moduleNameSymbol = check.requiredSymbol("module name", 1);
        String declaredName = moduleNameSymbol.stringValue();
        // TODO check null/empty

        SyntaxValue initialBindingsStx =
            check.requiredForm("initial module path", 2);

        // The new namespace shares the registry of current-namespace
        Namespace moduleNamespace =
            eval.newEmptyNamespace(envOutsideModule.namespace());

        SyntaxWrap moduleWrap = new EnvironmentRenameWrap(moduleNamespace);

        SyntaxWrap initialWrap;
        try
        {
            ModuleIdentity initialBindingsId =
                myModuleNameResolver.resolve(eval, initialBindingsStx);
            ModuleInstance initial =
                moduleNamespace.getRegistry().lookup(initialBindingsId);
            initialWrap = new ModuleRenameWrap(initial);
        }
        catch (FusionException e)
        {
            String message =
                "Error installing initial bindings: " + e.getMessage();
            throw check.failure(message);
        }


        // Pass 1: locate definitions and install dummy bindings

        ArrayList<SyntaxSexp> provideForms = new ArrayList<SyntaxSexp>();
        ArrayList<SyntaxValue> otherForms = new ArrayList<SyntaxValue>();

        for (int i = 3; i < source.size(); i++)
        {
            SyntaxValue form = source.get(i);
            form.addWrap(initialWrap);
            form.addWrap(moduleWrap);

            SyntaxSexp provide = formIsProvide(form);
            if (provide != null)
            {
                provideForms.add(provide);
            }
            else
            {
                SyntaxValue expanded;
                if (form instanceof SyntaxSexp)
                {
                    expanded =
                        ((SyntaxSexp)form).partialExpand(eval, moduleNamespace,
                                                         myPartialExpansionStops);
                    if (expanded instanceof SyntaxSexp)
                    {
                        SyntaxSexp sexp = (SyntaxSexp)expanded;
                        Binding binding = firstBinding(sexp);

                        if (binding == myDefineBinding)
                        {
                            String name =
                                DefineKeyword.boundName(eval, moduleNamespace,
                                                        sexp);
                            moduleNamespace.predefine(name);
                        }
                        else if (binding == myDefineSyntaxBinding)
                        {
                            try
                            {
                                eval.prepareAndEval(moduleNamespace, expanded);
                            }
                            catch (FusionException e)
                            {
                                String message = e.getMessage();
                                throw new SyntaxFailure("define_syntax",
                                                        message, form);
                            }
                            // TODO shouldn't need to keep this for later,
                            // but we throw away all this work at the moment
                            // and do it all again during invoke()
//                          expanded = null;
                        }
                        else if (binding == myUseSyntaxBinding)
                        {
                            try
                            {
                                eval.prepareAndEval(moduleNamespace, expanded);
                            }
                            catch (FusionException e)
                            {
                                String message = e.getMessage();
                                throw new SyntaxFailure("use",
                                                        message, form);
                            }
                            // TODO shouldn't need to keep this for later,
                            // but we throw away all this work at the moment
                            // and do it all again during invoke()
//                          expanded = null;
                        }
                    }
                }
                else
                {
                    expanded = form.prepare(eval, moduleNamespace);
                }

                if (expanded != null) otherForms.add(expanded);
            }
        }

        // Pass 2: Expand the expressions. We also rearrange the forms,
        // but that's not really for any functional reason.

        ArrayList<SyntaxValue> subforms = new ArrayList<SyntaxValue>();
        subforms.add(source.get(0)); // module
        subforms.add(source.get(1)); // name
        subforms.add(source.get(2)); // language

        for (SyntaxValue stx : otherForms)
        {
            stx = stx.prepare(eval, moduleNamespace);
            subforms.add(stx);
        }

        subforms.addAll(provideForms);

        SyntaxSexp result = SyntaxSexp.make(source.getLocation(), subforms);
        return result;
    }


    Binding firstBinding(SyntaxSexp form)
    {
        if (form.size() != 0)
        {
            SyntaxValue first = form.get(0);
            if (first instanceof SyntaxSymbol)
            {
                Binding binding = ((SyntaxSymbol)first).getBinding();
                return binding;
            }
        }
        return null;
    }


    @Override
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp expr)
        throws FusionException
    {
        Namespace ns = env.namespace();
        Namespace moduleNamespace = eval.newEmptyNamespace(ns);

        try
        {
            SyntaxValue initialBindingsStx = expr.get(2);
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

        String declaredName = ((SyntaxSymbol) expr.get(1)).stringValue();
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
