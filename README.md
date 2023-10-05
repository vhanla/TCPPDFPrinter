# TCPPDFPrinter

TCPPDFPrinter is a Virtual Printer that turns PostScript RAW output into PDF files.

This is not driver application, since Windows drivers require digitally signing, at least since Windows Vista it is enforced other wise a special test signing mode would be required to allow that.

So this program reuses a AGPL PostScript processing Printer driver which is GhostScript.

## How does this works:

There are two programs in this project, a Windows service and TCP server which processes PostScript files with GhostScript.

- **The Windows Service:** just plays a persistent role, i.e. it makes sure the TCP server is running, for each Windows user logged in sessions, re-launching them if necessary and using the non elevated privileges of each logged in users.

- **The TCP server:** has more than one role, it acts a the GUI and the TCP server that process the incoming RAW  data sent from any printer to the localhost port service that this TCP server is listening.
This program has the following tasks:

	- Detect's if GhostScript PDF printer driver is installed (ghostpdf.inf)
	- Installs it requiring Elevated privileges if not already installed
	- If GhostScript PDF printer is available, it creates a new Custom Named PDF printer that sends RAW data to this TCP server.
	- It creates the Port for this PDF printer.
	- It also removes this printer and its port (this latter might require reboot on some cases)
	- This TCP server dumps the RAW data to a temporary file in order to process them with GhostScript's DLL and turn it into a PDF file.
	- Once processed, the PDF file is opened with the default or specified program.
	
## LICENSE

This program is Dual Licensed, a MIT License and AGPL License, and the last one if you decide to keep any GhostScript usage. 
The MIT License is active if you opt to another non AGPL printer driver, like the Microsoft's included Microsoft PS Class Driver which is included since Windows 8, and you would process the PS files with a different PDF conversion tool/library.
