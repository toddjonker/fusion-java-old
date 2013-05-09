// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.BindingDoc.COLLECT_DOCS_MARK;
import static com.amazon.fusion.FusionEval.evalSyntax;
import static com.amazon.fusion.GlobalState.DEFINE_SYNTAX;
import static com.amazon.fusion.Syntax.datumToSyntax;
import static com.amazon.ion.util.IonTextUtils.printQuotedSymbol;
import com.amazon.fusion.Namespace.NsBinding;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;

/**
 * @see <a href="http://docs.racket-lang.org/reference/module.html">Racket
 * modules</a>
 */
final class ModuleForm
    extends SyntacticForm
{
    private final DynamicParameter myCurrentModuleDeclareName;
    private final ModuleNameResolver myModuleNameResolver;

    ModuleForm(ModuleNameResolver moduleNameResolver,
               DynamicParameter currentModuleDeclareName)
    {
        //    "                                                                               |
        super("name body ...+",
              "Declares a module containing the given body. The NAME must be a symbol.");

        myCurrentModuleDeclareName = currentModuleDeclareName;
        myModuleNameResolver = moduleNameResolver;
    }


    @Override
    SyntaxValue expand(Expander expander, Environment envOutsideModule,
                       SyntaxSexp source)
        throws FusionException
    {
        SyntaxChecker check = check(source);
        if (! expander.isTopLevelContext())
        {
            throw check.failure("`module` declaration not at top-level");
        }


        final GlobalState globals = expander.getGlobalState();
        final Evaluator   eval    = expander.getEvaluator();

        SyntaxSymbol moduleNameSymbol = check.requiredSymbol("module name", 1);
        ModuleIdentity.validateLocalName(moduleNameSymbol);

        ModuleRegistry registry = envOutsideModule.namespace().getRegistry();

        ModuleIdentity initialBindingsId;
        ModuleInstance language;
        {
            SyntaxValue initialBindingsStx =
                check.requiredForm("initial module path", 2);
            try
            {
                initialBindingsId =
                    myModuleNameResolver.resolve(eval, initialBindingsStx);
                language = registry.lookup(initialBindingsId);
                assert language != null;  // Otherwise resolve should fail
            }
            catch (ModuleNotFoundFailure e)
            {
                e.addContext(initialBindingsStx);
                throw e;
            }
            catch (FusionException e)
            {
                String message =
                    "Error installing initial bindings: " + e.getMessage();
                SyntaxFailure ex =
                    new SyntaxFailure(getInferredName(), message,
                                      initialBindingsStx);
                ex.initCause(e);
                throw ex;
            }
        }

        // The new namespace shares the registry of current-namespace
        ModuleIdentity id;
        try
        {
            id = determineIdentity(eval, moduleNameSymbol);
        }
        catch (FusionException e)
        {
            String message =
                "Error determining module identity: " + e.getMessage();
            SyntaxFailure ex = check.failure(message);
            ex.initCause(e);
            throw ex;
        }

        ModuleNamespace moduleNamespace =
            new ModuleNamespace(registry, language, id);

        // TODO handle #%module-begin and #%plain-module-begin
        expander = expander.enterModuleContext();


        // Pass 1: locate definitions and install dummy bindings

        // TODO FUSION-38 Imported names scope should be the whole module.
        // Probably need to have the module-level wrap extended with the
        // imports, so we don't have to backtrack and add more wraps to
        // earlier forms.  Or maybe scan for the imports first?

        ArrayList<SyntaxSexp>  provideForms      = new ArrayList<>();
        ArrayList<SyntaxValue> otherForms        = new ArrayList<>();
        ArrayList<Boolean>     preparedFormFlags = new ArrayList<>();

        LinkedList<SyntaxValue> forms = new LinkedList<>();
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

                SyntaxValue expanded =
                    expander.partialExpand(moduleNamespace, form);

                if (expanded instanceof SyntaxSexp)
                {
                    SyntaxSexp sexp = (SyntaxSexp)expanded;
                    Binding binding = sexp.firstBinding();

                    if (binding == globals.myKernelDefineBinding)
                    {
                        expanded = DefineForm.predefine(eval, moduleNamespace,
                                                        sexp, form);
                    }
                    else if (binding == globals.myKernelDefineSyntaxBinding)
                    {
                        try
                        {
                            expanded =
                                expander.expand(moduleNamespace, expanded);
                            // TODO this is getting compiled twice
                            CompiledForm compiled =
                                eval.compile(moduleNamespace, expanded);
                            eval.eval(moduleNamespace, compiled);
                        }
                        catch (SyntaxFailure e)
                        {
                            e.addContext(form);
                            throw e;
                        }
                        catch (FusionException e)
                        {
                            String message = e.getMessage();
                            throw new SyntaxFailure(DEFINE_SYNTAX,
                                                    message, form);
                        }
                        formIsPrepared = true;
                    }
                    else if (binding == globals.myKernelUseBinding ||
                             binding == globals.myKernelRequireBinding)
                    {
                        try
                        {
                            evalSyntax(eval, expanded, moduleNamespace);
                        }
                        catch (FusionException e)
                        {
                            String message = e.getMessage();
                            SyntaxFailure ex =
                                new SyntaxFailure(binding.getName(),
                                                  message, form);
                            ex.initCause(e);
                            throw ex;
                        }
                        expanded = null;
                    }
                    else if (binding == globals.myKernelBeginBinding)
                    {
                        // Splice 'begin' into the module-begin sequence
                        int last = sexp.size() - 1;
                        for (int i = last; i != 0;  i--)
                        {
                            forms.push(sexp.get(i));
                        }
                        formsAlreadyWrapped += last;
                        expanded = null;
                    }
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
            new SyntaxValue[4 + otherForms.size() + provideForms.size()];

        int i = 0;
        subforms[i++] = source.get(0); // module
        subforms[i++] = source.get(1); // name
        subforms[i++] = source.get(2); // language

        // FIXME a ludicrous hack to communicate this data to the compiler!
        {
            SyntaxInt variableCount =
                SyntaxInt.make(moduleNamespace.getBindings().size());

            SyntaxString identity =
                SyntaxString.make(id.internString());

            SyntaxString languageIdentity =
                SyntaxString.make(initialBindingsId.internString());

            SyntaxStruct meta =
                SyntaxStruct.make(new String[] { "variable_count",
                                                 "identity",
                                                 "language_identity" },
                                  new SyntaxValue[] { variableCount,
                                                      identity,
                                                      languageIdentity },
                                  new String[] { "InjectedMetadata" });

            subforms[i++] = meta;
        }

        Iterator<Boolean> prepared = preparedFormFlags.iterator();
        for (SyntaxValue stx : otherForms)
        {
            if (! prepared.next())
            {
                if (firstBinding(stx) == globals.myKernelDefineBinding)
                {
                    assert expander.isModuleContext();
                    stx = expander.expand(moduleNamespace, stx);
                }
                else
                {
                    stx = expander.expandExpression(moduleNamespace, stx);
                }
            }
            subforms[i++] = stx;
        }

        for (SyntaxSexp stx : provideForms)
        {
            stx = expandProvide(eval, stx, moduleNamespace);
            subforms[i++] = stx;
        }

        SyntaxSexp result = SyntaxSexp.make(expander, source.getLocation(),
                                            subforms);
        return result;
    }

    Binding firstBinding(SyntaxValue stx)
    {
        if (stx instanceof SyntaxSexp)
        {
            return ((SyntaxSexp) stx).firstBinding();
        }
        return null;
    }


    @Override
    CompiledForm compile(Evaluator eval, Environment envOutsideModule,
                         SyntaxSexp stx)
        throws FusionException
    {
        // TODO We repeat work here that was done during expansion.
        // Racket stashes the necessary data in syntax properties.
        // See Racket reference 11.9.1
        // http://docs.racket-lang.org/reference/Expanding_Top-Level_Forms.html#%28part._modinfo%29

        SyntaxStruct meta = (SyntaxStruct) stx.get(3);

        ModuleIdentity id;
        {
            SyntaxText form = (SyntaxText) meta.get(eval, "identity");
            String identity = form.stringValue();
            id = ModuleIdentity.reIntern(identity);
        }

        Namespace moduleNamespace;
        {
            SyntaxText form = (SyntaxText) meta.get(eval, "language_identity");
            String identity = form.stringValue();
            ModuleIdentity languageId = ModuleIdentity.reIntern(identity);

            ModuleRegistry registry =
                envOutsideModule.namespace().getRegistry();
            ModuleInstance language = registry.lookup(languageId);

            moduleNamespace = new ModuleNamespace(registry, language, id);
        }

        int variableCount;
        {
            SyntaxInt form = (SyntaxInt) meta.get(eval, "variable_count");
            variableCount = form.bigIntegerValue().intValue();
        }

        ArrayList<SyntaxSexp> provideForms = new ArrayList<SyntaxSexp>();
        ArrayList<CompiledForm> otherForms = new ArrayList<CompiledForm>();

        int bodyPos = 4;
        String docs = null;
        if (stx.size() > 5 && stx.get(4) instanceof SyntaxString)
        {
            // We're gonna call this documentation!
            bodyPos++;
            if (eval.firstContinuationMark(COLLECT_DOCS_MARK) != null)
            {
                docs = ((SyntaxString) stx.get(4)).stringValue();
            }
        }

        for (int i = bodyPos; i < stx.size(); i++)
        {
            SyntaxValue form = stx.get(i);
            SyntaxSexp provide = formIsProvide(form);
            if (provide != null)
            {
                provideForms.add(provide);
            }
            else
            {
                CompiledForm compiled = eval.compile(moduleNamespace, form);
                otherForms.add(compiled);
            }
        }

        SyntaxSymbol[] providedIdentifiers =
            providedSymbols(eval, moduleNamespace, provideForms);

        CompiledForm[] otherFormsArray =
            otherForms.toArray(new CompiledForm[otherForms.size()]);

        return new CompiledModule(id,
                                  docs,
                                  moduleNamespace.requiredModuleIds(),
                                  variableCount,
                                  moduleNamespace.extractBindingDocs(),
                                  providedIdentifiers, otherFormsArray);
    }

    private ModuleIdentity determineIdentity(Evaluator eval,
                                             SyntaxSymbol moduleNameSymbol)
        throws FusionException
    {
        ModuleIdentity id;
        String current = myCurrentModuleDeclareName.asString(eval);
        if (current != null)
        {
            id = ModuleIdentity.reIntern(current);
        }
        else
        {
            ModuleRegistry reg = eval.findCurrentNamespace().getRegistry();
            String declaredName = moduleNameSymbol.stringValue();
            id = ModuleIdentity.internLocalName(reg, declaredName);
        }
        return id;
    }


    //========================================================================


    private SyntaxSexp formIsProvide(SyntaxValue form)
    {
        // TODO FUSION-135 this needs to use binding comparison, not strcmp
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
     * Performs expand-time syntax checking of {@code provide} forms.
     * Note that we expand these after expanding the rest of the module.
     */
    private SyntaxSexp expandProvide(Evaluator eval,
                                     SyntaxSexp form,
                                     Namespace moduleNamespace)
        throws FusionException
    {
        ArrayList<SyntaxValue> expanded = new ArrayList<>(form.size());
        expanded.add(form.get(0));

        SyntaxChecker check = new SyntaxChecker("provide", form);
        for (int i = 1; i < form.size(); i++)
        {
            SyntaxValue spec = form.get(i);
            switch (spec.getType())
            {
                case SYMBOL:
                {
                    check.requiredIdentifier("bound identifier", i);

                    expandProvideId(moduleNamespace, (SyntaxSymbol) spec,
                                    check, expanded);
                    break;
                }
                case SEXP:
                {
                    expandProvideSexp(eval, moduleNamespace,
                                      (SyntaxSexp) spec,
                                      check.subformSexp("provide-spec", i),
                                      expanded);
                    break;
                }
                default:
                {
                    throw check.failure("expected provide-spec", form.get(i));
                }
            }
        }

        SyntaxValue[] children =
            expanded.toArray(new SyntaxValue[expanded.size()]);
        return SyntaxSexp.make(eval, children, form.getAnnotations(), form.getLocation());
    }


    private void expandProvideId(Namespace moduleNamespace,
                                 SyntaxSymbol identifier,
                                 SyntaxChecker check,
                                 ArrayList<SyntaxValue> expanded)
        throws FusionException
    {
        String publicName = identifier.stringValue();

        // TODO FUSION-139 id.resolve works when the id has the ns in context
        // It doesn't work when the ns isn't in context because the
        // provided binding was macro-introduced.

        Binding b = identifier.resolve();
        // Binding local = moduleNamespace.localResolve(identifier);
        // localResolve isn't right either since it doesn't find imports

        if (b instanceof FreeBinding)
        {
            String message =
                "cannot export " + printQuotedSymbol(publicName) +
                " since it has no definition.";
            throw check.failure(message);
        }

        String freeName = b.getName();
        if (! publicName.equals(freeName))
        {
            String message =
                "cannot export binding since symbolic name " +
                printQuotedSymbol(publicName) +
                " differs from resolved name " +
                printQuotedSymbol(freeName);
            throw check.failure(message);
        }

        expanded.add(identifier);
    }

    private void expandProvideSexp(Evaluator eval,
                                   Namespace moduleNamespace,
                                   SyntaxSexp specForm,
                                   SyntaxChecker check,
                                   ArrayList<SyntaxValue> expanded)
        throws FusionException
    {
        SyntaxSymbol tag = check.requiredIdentifier(0);
        switch (tag.stringValue())
        {
            case "all_defined_out":
            {
                check.arityExact(1);

                // Filter by lexical context: we shouldn't export identifiers
                // introduced by macros unless this form was also introduced
                // at the same time.
                for (NsBinding binding : moduleNamespace.getBindings())
                {
                    // TODO the datum->syntax context should be the whole sexp
                    // form `(all_defined_out)` not just `all_defined_out` but
                    // we don't currently retain context on SyntaxSexp after
                    // it has been pushed down to children.
                    SyntaxSymbol localized = (SyntaxSymbol)
                        datumToSyntax(eval,
                                      SyntaxSymbol.make(binding.getName()),
                                      tag);
                    localized = localized.copyAndResolveTop();
                    Binding localBinding =
                        moduleNamespace.localResolve(localized);
                    if (localBinding != null
                        && binding.sameTarget(localBinding))
                    {
                        localized = localized.copyReplacingBinding(binding);
                        expanded.add(localized);
                    }

                    // TODO FUSION-136 handle rename-transformers per Racket
                }

                break;
            }
            default:
            {
                throw check.failure("invalid provide-spec");
            }
        }
    }


    /**
     * @return not null.
     */
    private SyntaxSymbol[] providedSymbols(Evaluator eval,
                                           Namespace ns,
                                           ArrayList<SyntaxSexp> provideForms)
        throws FusionException
    {
        ArrayList<SyntaxSymbol> identifiers = new ArrayList<SyntaxSymbol>();

        for (SyntaxSexp form : provideForms)
        {
            SyntaxChecker check = new SyntaxChecker("provide", form);

            int size = form.size();
            for (int i = 1; i < size; i++)
            {
                SyntaxValue spec = form.get(i);
                switch (spec.getType())
                {
                    case SYMBOL:
                    {
                        SyntaxSymbol id = (SyntaxSymbol) spec;
                        identifiers.add(id);
                        break;
                    }
                    default:
                    {
                        throw check.failure("invalid provide-spec");
                    }
                }
            }
        }

        return identifiers.toArray(new SyntaxSymbol[identifiers.size()]);
    }


    //========================================================================


    private static final class CompiledModule
        implements CompiledForm
    {
        private final ModuleIdentity   myId;
        private final String           myDocs;
        private final ModuleIdentity[] myRequiredModules;
        private final int              myVariableCount;
        private final BindingDoc[]     myBindingDocs;
        private final SyntaxSymbol[]   myProvidedIdentifiers;
        private final CompiledForm[]   myBody;

        private CompiledModule(ModuleIdentity   id,
                               String           docs,
                               ModuleIdentity[] requiredModules,
                               int              variableCount,
                               BindingDoc[]     bindingDocs,
                               SyntaxSymbol[]   providedIdentifiers,
                               CompiledForm[]   body)
        {
            myId                  = id;
            myDocs                = docs;
            myRequiredModules     = requiredModules;
            myVariableCount       = variableCount;
            myBindingDocs         = bindingDocs;
            myProvidedIdentifiers = providedIdentifiers;
            myBody                = body;
        }

        @Override
        public Object doEval(Evaluator eval, Store ignored)
            throws FusionException
        {
            ModuleRegistry registry =
                eval.findCurrentNamespace().getRegistry();

            ModuleStore[] requiredModuleStores = requireModules(registry);

            // Allocate just enough space for the top-level bindings.
            ModuleStore store =
                new ModuleStore(registry, requiredModuleStores,
                                myVariableCount, myBindingDocs);

            ModuleInstance module =
                new ModuleInstance(myId, myDocs, store, myProvidedIdentifiers);

            eval.findCurrentNamespace().getRegistry().register(module);

            for (CompiledForm form : myBody)
            {
                eval.eval(store, form);
            }

            return module;
        }

        private ModuleStore[] requireModules(ModuleRegistry registry)
        {
            int count = myRequiredModules.length;
            ModuleStore[] stores = new ModuleStore[count];
            for (int i = 0; i < count; i++)
            {
                ModuleInstance module = registry.lookup(myRequiredModules[i]);
                stores[i] = module.getNamespace();
            }

            return stores;
        }
    }
}
