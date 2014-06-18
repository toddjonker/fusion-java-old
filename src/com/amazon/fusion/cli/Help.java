// Copyright (c) 2005-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import java.io.IOException;

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
        "Usage: fusion [OPTIONS ...] <command> [ARGS ...]\n" +
        "Type 'fusion help <command>' for help on a specific command.\n\n" +
        "Available commands:\n";

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
    Executor makeExecutor(String[] arguments)
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
            throws UsageException, IOException
        {
            System.out.println();

            if ((myCommands == null) || (myCommands.length == 0))
            {
                renderGeneralHelp(System.out);
            }
            else
            {
                renderCommands(System.out);
            }

            return 0;
        }


        private void renderGeneralHelp(Appendable out)
            throws IOException
        {
            out.append(APP_HELP_TEXT_INTRO);

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

            out.append(GlobalOptions.HELP);
        }


        private void renderCommands(Appendable out)
            throws UsageException, IOException
        {
            boolean showGeneralUsage = false;

            for (int i = 0; i < myCommands.length; i++)
            {
                if (i != 0)
                {
                    out.append(FULL_HELP_SEPARATOR);
                }

                String command = myCommands[i];

                Command commandObj =
                    CommandFactory.getMatchingCommand(command);
                if (commandObj == null)
                {
                    out.append("Unknown command: '");
                    out.append(command);
                    out.append("'\n");
                    showGeneralUsage = true;
                }
                else
                {
                    renderFullHelp(commandObj, out);
                }
            }

            if (showGeneralUsage)
            {
                throw new UsageException(null);
            }
        }


        private void renderCommandAndAliases(Command command, Appendable out)
            throws IOException
        {
            out.append(command.getCommand());

            String[] aliases = command.getAliases();
            int len = aliases.length;
            if (len != 0)
            {
                out.append(" (");
                for (int i = 0; i < len; i++ )
                {
                    if (i != 0) out.append(", ");
                    out.append(aliases[i]);
                }
                out.append(")");
            }
        }


        private void renderFullHelp(Command command, Appendable out)
            throws IOException
        {
            String oneLiner = command.getHelpOneLiner();
            if (oneLiner != null)
            {
                renderCommandAndAliases(command, out);
                out.append('\n');
                //            out.print(": ");
                out.append("  ");
                out.append(oneLiner);
                out.append("\n\n");
                out.append("Usage: ");
                out.append(command.getHelpUsage());
                out.append('\n');
            }

            String helpBody = command.getHelpBody();
            if (helpBody != null)
            {
                out.append('\n');
                out.append(helpBody);
                out.append('\n');
            }
        }
    }
}
