// Copyright (c) 2005-2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import java.io.PrintWriter;

class Help
    extends Command
{
    private static final String HELP_ONE_LINER =
        "Describe the usage of this program or its commands.";

    private static final String HELP_USAGE =
        "help [COMMAND ...]";

    private static final String HELP_BODY =
        "With no arguments, print the complete list of commands along with brief\n" +
        "descriptions.  If commands are specified, print the full help information\n" +
        "for each one.";

    private final static String APP_HELP_TEXT_INTRO =
        "Usage: fusion <command> [ARGS ...]\n" +
        "Type 'fusion help <command>' for help on a specific command.\n\n" +
        "Available commands:";

    private final static String FULL_HELP_SEPARATOR =
        "\n                                *      *      *\n\n";


    //=========================================================================
    // Constructors

    Help()
    {
        super("help", new String[] { "?", "h" });
        putHelpText(HELP_ONE_LINER, HELP_USAGE, HELP_BODY);
    }


    //=========================================================================


    @Override
    Executor processArguments(String[] arguments)
    {
        return new Executor(arguments);
    }


    private static class Executor
        implements Command.Executor
    {
        private final String[] myCommands;

        private Executor(String[] commands)
        {
            myCommands = commands;
        }


        @Override
        public int execute()
        {
            PrintWriter out = new PrintWriter(System.out);

            try
            {
                if ((myCommands == null) || (myCommands.length == 0))
                {
                    out.println(APP_HELP_TEXT_INTRO);

                    TablePrinter table = new TablePrinter();
                    table.setIndent(2);

                    Command[] allCommands = CommandFactory.getAllCommands();
                    for (int i = 0; i < allCommands.length; i++)
                    {
                        Command command = allCommands[i];

                        String oneLiner = command.getHelpOneLiner();
                        if (oneLiner != null)
                        {
                            String[] row = { command.getCommand(), oneLiner };

                            table.addRow(row);
                        }
                    }

                    table.render(out);
                }
                else
                {
                    for (int i = 0; i < myCommands.length; i++)
                    {
                        if (i != 0)
                        {
                            out.print(FULL_HELP_SEPARATOR);
                        }
                        String command = myCommands[i];

                        Command commandObj =
                            CommandFactory.getMatchingCommand(command);
                        if (commandObj == null)
                        {
                            out.println("Unknown command: '" + command + "'");
                        }
                        else
                        {
                            renderFullHelp(commandObj, out);
                        }
                    }
                }
            }
            finally
            {
                out.close();
            }

            return 0;
        }


        private void renderCommandAndAliases(Command command, PrintWriter out)
        {
            out.print(command.getCommand());

            String[] aliases = command.getAliases();
            int len = aliases.length;
            if (len != 0)
            {
                out.print(" (");
                for (int i = 0; i < len; i++ )
                {
                    if (i != 0) out.print(", ");
                    out.print(aliases[i]);
                }
                out.print(")");
            }
        }


        private void renderFullHelp(Command command, PrintWriter out)
        {
            String oneLiner = command.getHelpOneLiner();
            if (oneLiner != null)
            {
                renderCommandAndAliases(command, out);
                out.println();
                //            out.print(": ");
                out.print("  ");
                out.println(oneLiner);
                out.println();
                out.print("Usage: ");
                out.println(command.getHelpUsage());
            }

            String helpBody = command.getHelpBody();
            if (helpBody != null)
            {
                out.println();
                out.println(helpBody);
            }
        }
    }
}
