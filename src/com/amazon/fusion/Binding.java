// Copyright (c) 2012-2016 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.FusionSymbol.BaseSymbol;
import com.amazon.fusion.LocalEnvironment.LocalBinding;
import com.amazon.fusion.ModuleNamespace.ProvidedBinding;
import com.amazon.fusion.Namespace.NsDefinedBinding;
import com.amazon.fusion.Namespace.RequiredBinding;

/**
 * Bindings are used during expansion and compilation to identify a specific
 * binding site.
 * They are compiled away and are not used at eval-time.
 * <p>
 * The types of bindings are as follows:
 * <ul>
 *   <li><em>Original</em> bindings represent the base cases for bindings.
 *     <ul>
 *       <li><em>Physical</em> bindings denote variables and map to storage
 *         locations.
 *         <ul>
 *           <li>{@link NsDefinedBinding}s denote namespace-level variables.
 *           </li>
 *           <li>{@link LocalBinding}s denote variables at other levels, like
 *             the arguments to procedures and the local variables declared by
 *             {@code let}, {@code letrec}, <i>etc.</i>
 *           </li>
 *         </ul>
 *       </li>
 *       <li><em>Free</em> bindings do not map to anything, they are essentially
 *         unbound variables.
 *       </li>
 *     </ul>
 *   </li>
 *   <li><em>Indirect</em> bindings refer to an original binding indirectly,
 *     perhaps renaming the variable along the way.
 *     <ul>
 *       <li>{@link ProvidedBinding}s refer to module-level defined
 *         bindings.
 *       </li>
 *       <li>{@link RequiredBinding}s refer to provided bindings.
 *       </li>
 *     </ul>
 *   </li>
 * </ul>
 *
 * Every binding has a <em>target</em> binding, which is the original binding
 * to which it refers, perhaps through multiple indirections.
 * A binding is its own target if and only if the binding is original.
 */
abstract class Binding
{
    abstract BaseSymbol getName();

    /**
     * Determines whether this is a {@link FreeBinding} with the given name.
     * The default implementation returns false.
     */
    boolean isFree(BaseSymbol name)
    {
        return false;
    }

    /**
     * Gets the original binding to which this binding refers.
     * Returns itself, if and only if this binding is original.
     *
     * @return not null.
     */
    Binding target()
    {
        return this;
    }

    /**
     * Determines whether two bindings refer to the same {@link #target()},
     * despite any renames on import or export.
     *
     * @param other must not be null.
     */
    boolean sameTarget(Binding other)
    {
        return target() == other.target();
    }

    ProvidedBinding provideAs(BaseSymbol name)
    {
        throw new IllegalStateException("This kind of binding can't be exported");
    }

    /**
     * Don't call directly! Second half of double-dispatch from
     * {@link Namespace#lookup(Binding)}.
     *
     * @return null if there's no value associated with the binding.
     */
    abstract Object lookup(Namespace ns);

    /**
     * Checks whether this binding can be mutated.
     *
     * @return null if the binding can be mutated, else a syntax error message.
     */
    String mutationSyntaxErrorMessage()
    {
        return null;
    }


    CompiledForm compileDefine(Evaluator eval,
                               Environment env,
                               SyntaxSymbol id,
                               CompiledForm valueForm)
        throws FusionException
    {
        throw new IllegalStateException("Unexpected `define` context.");
    }


    /** Compile a reference to the variable denoted by this binding. */
    abstract CompiledForm compileReference(Evaluator eval,
                                           Environment env)
        throws FusionException;

    /** Compile a #%top reference. */
    abstract CompiledForm compileTopReference(Evaluator eval,
                                              Environment env,
                                              SyntaxSymbol id)
        throws FusionException;

    /** Compile a mutation of the variable denoted by this binding. */
    abstract CompiledForm compileSet(Evaluator eval,
                                     Environment env,
                                     CompiledForm valueForm)
        throws FusionException;
}
