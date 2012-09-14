// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.ion.util.IonTextUtils.printQuotedSymbol;
import static java.lang.Boolean.TRUE;
import com.amazon.fusion.ModuleInstance.ModuleBinding;
import com.amazon.fusion.Namespace.NsBinding;
import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;

/**
 * @see <a href="http://docs.racket-lang.org/reference/module.html">Racket
 * modules</a>
 */
final class ModuleKeyword
    extends KeywordValue
{
    private final DynamicParameter myCurrentModuleDeclareName;
    private final ModuleNameResolver myModuleNameResolver;

    ModuleKeyword(ModuleNameResolver moduleNameResolver,
                  DynamicParameter currentModuleDeclareName,
                  Namespace kernelNamespace)
    {
        //    "                                                                               |
        super("NAME BODY ...+",
              "Declares a module containing the given body. The NAME must be a symbol.");

        myCurrentModuleDeclareName = currentModuleDeclareName;
        myModuleNameResolver = moduleNameResolver;
    }


    private Binding stopBinding(ModuleInstance kernel,
                                Map<Binding, Object> stops,
                                String name)
    {
        Binding b = kernel.resolveProvidedName(name);
        assert b != null;
        stops.put(b, TRUE);
        return b;
    }


    @Override
    SyntaxValue expand(Evaluator eval,
                        Environment envOutsideModule,
                        SyntaxSexp source)
        throws SyntaxFailure
    {
        SyntaxChecker check = check(source);

        ModuleInstance kernel = eval.findKernel();

        // TODO precompute this?
        IdentityHashMap<Binding, Object> stops =
            new IdentityHashMap<Binding, Object>();
        Binding defineBinding       = stopBinding(kernel, stops, "define");
        Binding defineSyntaxBinding = stopBinding(kernel, stops, "define_syntax");
        Binding useSyntaxBinding    = stopBinding(kernel, stops, "use");
        Binding beginBinding        = stopBinding(kernel, stops, "begin");

        SyntaxSymbol moduleNameSymbol = check.requiredSymbol("module name", 1);
        String declaredName = moduleNameSymbol.stringValue();
        // TODO check null/empty

        SyntaxValue initialBindingsStx =
            check.requiredForm("initial module path", 2);

        ModuleRegistry registry = envOutsideModule.namespace().getRegistry();

        ModuleInstance language;
        try
        {
            ModuleIdentity initialBindingsId =
                myModuleNameResolver.resolve(eval, initialBindingsStx);
            language = registry.lookup(initialBindingsId);
        }
        catch (FusionException e)
        {
            String message =
                "Error installing initial bindings: " + e.getMessage();
            throw check.failure(message);
        }

        // The new namespace shares the registry of current-namespace
        ModuleIdentity id;
        try
        {
            id = determineIdentity(eval, declaredName);
        }
        catch (FusionException e)
        {
            String message =
                "Error determining module identity: " + e.getMessage();
            throw check.failure(message);
        }

        Namespace moduleNamespace =
            new ModuleNamespace(registry, language, id);

        // TODO handle #%module-begin and #%plain-module-begin

        // Pass 1: locate definitions and install dummy bindings

        // TODO FUSION-38 Imported names scope should be the whole module.
        // Probably need to have the module-level wrap extended with the
        // imports, so we don't have to backtrack and add more wraps to
        // earlier forms.  Or maybe scan for the imports first?

        ArrayList<SyntaxSexp> provideForms = new ArrayList<SyntaxSexp>();
        ArrayList<SyntaxValue> otherForms = new ArrayList<SyntaxValue>();
        ArrayList<Boolean> preparedFormFlags = new ArrayList<Boolean>();

        LinkedList<SyntaxValue> forms = new LinkedList<SyntaxValue>();
        source.extract(forms, 3);

        int formsAlreadyWrapped = 0;
        while (! forms.isEmpty())
        {
            SyntaxValue form = forms.pop();
            if (formsAlreadyWrapped == 0)
            {
                // This is ugly, but we don't want to do this twice. Forms
                // that were spliced by 'begin' have already had this done.
                form = moduleNamespace.syntaxIntroduce(form);
            }
            else
            {
                formsAlreadyWrapped--;
            }

            SyntaxSexp provide = formIsProvide(form);
            if (provide != null)
            {
                provideForms.add(provide);
            }
            else
            {
                boolean formIsPrepared = false;
                SyntaxValue expanded;
                if (form instanceof SyntaxSexp)
                {
                    expanded =
                        ((SyntaxSexp)form).partialExpand(eval, moduleNamespace,
                                                         stops);
                    if (expanded instanceof SyntaxSexp)
                    {
                        SyntaxSexp sexp = (SyntaxSexp)expanded;
                        Binding binding = firstBinding(sexp);

                        if (binding == defineBinding)
                        {
                            SyntaxSymbol identifier =
                                DefineKeyword.boundIdentifier(eval,
                                                              moduleNamespace,
                                                              sexp);
                            identifier = identifier.stripImmediateEnvWrap(moduleNamespace);
                            moduleNamespace.predefine(identifier);
                        }
                        else if (binding == defineSyntaxBinding)
                        {
                            try
                            {
                                expanded =
                                    expanded.expand(eval, moduleNamespace);
                                // TODO this is getting compiled twice
                                CompiledForm compiled =
                                    eval.compile(moduleNamespace, expanded);
                                eval.eval(moduleNamespace, compiled);
                            }
                            catch (FusionException e)
                            {
                                String message = e.getMessage();
                                throw new SyntaxFailure("define_syntax",
                                                        message, form);
                            }
                            formIsPrepared = true;
                        }
                        else if (binding == useSyntaxBinding)
                        {
                            try
                            {
                                eval.prepareAndEvalTopLevelForm(expanded,
                                                                moduleNamespace);
                            }
                            catch (FusionException e)
                            {
                                String message = e.getMessage();
                                throw new SyntaxFailure("use",
                                                        message, form);
                            }
                            expanded = null;
                        }
                        else if (binding == beginBinding)
                        {
                            // Top-level 'begin' is spliced into the module
                            int last = sexp.size() - 1;
                            for (int i = last; i != 0;  i--)
                            {
                                forms.push(sexp.get(i));
                            }
                            formsAlreadyWrapped += last;
                            expanded = null;
                        }
                    }
                }
                else
                {
                    expanded = form;
                }

                if (expanded != null)
                {
                    otherForms.add(expanded);
                    preparedFormFlags.add(formIsPrepared);
                }
            }
        }

        // Pass 2: Expand the expressions. We also rearrange the forms,
        // but that's not really for any functional reason.

        SyntaxValue[] subforms =
            new SyntaxValue[3 + otherForms.size() + provideForms.size()];
        subforms[0] = source.get(0); // module
        subforms[1] = source.get(1); // name
        subforms[2] = source.get(2); // language
        int i = 3;

        Iterator<Boolean> prepared = preparedFormFlags.iterator();
        for (SyntaxValue stx : otherForms)
        {
            if (! prepared.next())
            {
                stx = stx.expand(eval, moduleNamespace);
            }
            subforms[i++] = stx;
        }

        for (SyntaxSexp stx : provideForms)
        {
            stx = prepareProvide(stx, moduleNamespace);
            subforms[i++] = stx;
        }

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
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp expr)
        throws FusionException
    {
        // TODO We repeat work here that was done during expansion.
        // Racket stashed the necessary data in syntax properties.
        // See Racket reference 11.9.1
        // http://docs.racket-lang.org/reference/Expanding_Top-Level_Forms.html#%28part._modinfo%29

        ModuleIdentity initialBindingsId;
        try
        {
            SyntaxValue initialBindingsStx = expr.get(2);
            initialBindingsId =
                myModuleNameResolver.resolve(eval, initialBindingsStx);
        }
        catch (FusionException e)
        {
            String message =
                "Error resolving initial-bindings module: " + e.getMessage();
            throw new FusionException(message, e);
        }

        ArrayList<SyntaxSexp> provideForms = new ArrayList<SyntaxSexp>();
        ArrayList<CompiledForm> otherForms = new ArrayList<CompiledForm>();

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
                CompiledForm compiled = eval.compile(env, form);
                otherForms.add(compiled);
            }
        }

        SyntaxSymbol[] providedIdentifiers = providedSymbols(provideForms);

        CompiledForm[] otherFormsArray =
            otherForms.toArray(new CompiledForm[otherForms.size()]);

        String declaredName = ((SyntaxSymbol) expr.get(1)).stringValue();
        ModuleIdentity id = determineIdentity(eval, declaredName);

        return new CompiledModule(id, initialBindingsId, providedIdentifiers,
                                  otherFormsArray);
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


    //========================================================================


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
     * @param myNamespace
     * @return
     */
    private SyntaxSexp prepareProvide(SyntaxSexp form,
                                      Namespace moduleNamespace)
        throws SyntaxFailure
    {
        int size = form.size();
        SyntaxChecker check = new SyntaxChecker("provide", form);
        for (int i = 1; i < size; i++)
        {
            check.requiredNonEmptySymbol("bound identifier", i);

            SyntaxSymbol identifier = (SyntaxSymbol) form.get(i);
            String publicName = identifier.stringValue();

            String freeName;
            Binding b = identifier.resolve();
            if (b instanceof NsBinding)
            {
                freeName = ((NsBinding)b).getName();
            }
            else if (b instanceof ModuleBinding)
            {
                freeName = ((ModuleBinding)b).getName();
            }
            else
            {
                assert b instanceof FreeBinding;

                String message =
                    "cannot export " + printQuotedSymbol(publicName) +
                    " since it has no definition.";
                throw check.failure(message);
            }

            if (! publicName.equals(freeName))
            {
                String message =
                    "cannot export binding since symbolic name " +
                    printQuotedSymbol(publicName) +
                    " differs from resolved name " +
                    printQuotedSymbol(freeName);
                throw check.failure(message);
            }
        }

        return form;
    }


    /**
     * @return not null.
     */
    private SyntaxSymbol[] providedSymbols(ArrayList<SyntaxSexp> provideForms)
        throws SyntaxFailure
    {
        ArrayList<SyntaxSymbol> identifiers = new ArrayList<SyntaxSymbol>();

        for (SyntaxSexp form : provideForms)
        {
            int size = form.size();
            for (int i = 1; i < size; i++)
            {
                SyntaxSymbol identifier = (SyntaxSymbol) form.get(i);
                identifiers.add(identifier);
            }
        }

        return identifiers.toArray(new SyntaxSymbol[identifiers.size()]);
    }


    //========================================================================


    private static final class CompiledModule
        implements CompiledForm
    {
        private final ModuleIdentity myId;
        private final ModuleIdentity myInitialBindingsId;
        private final SyntaxSymbol[] myProvidedIdentifiers;
        private final CompiledForm[] myBody;

        private CompiledModule(ModuleIdentity id,
                               ModuleIdentity initialBindingsId,
                               SyntaxSymbol[] providedIdentifiers,
                               CompiledForm[] body)
        {
            myId = id;
            myInitialBindingsId = initialBindingsId;
            myProvidedIdentifiers = providedIdentifiers;
            myBody = body;
        }

        @Override
        public FusionValue doEval(Evaluator eval, Store store)
            throws FusionException
        {
            // TODO Use the original Bindings to populate the namespace?
            // Allocate just enough space for the top-level bindings.
            ModuleRegistry registry = eval.getModuleRegistry();
            ModuleNamespace namespace = new ModuleNamespace(registry, myId);
            namespace.use(myInitialBindingsId);

            ModuleInstance module =
                new ModuleInstance(myId, namespace, myProvidedIdentifiers);
            registry.register(module);

            for (CompiledForm form : myBody)
            {
                eval.eval(namespace, form);
            }

            return module;
        }
    }
}
