# Nome do executável
EXECUTABLE = Main

# Diretório para os arquivos intermediários
OUTPUT_DIR = ./output

# Caminhos dos arquivos
SOURCE_FILES = Main.hs
MODULE_FILES = utils/Matrix.hs

# Comando para compilar o código
GHCCOMPILE = ghc -o $(EXECUTABLE) $(SOURCE_FILES) $(MODULE_FILES) -outputdir=$(OUTPUT_DIR) -hidir=$(OUTPUT_DIR) -odir=$(OUTPUT_DIR)

all:
	@echo "> Compiling..."
	$(GHCCOMPILE)
	@echo "> Compilation finished."

run: all
	./$(EXECUTABLE)

clean:
	@echo "> Cleaning output directory..."
	rm -rf $(OUTPUT_DIR)
	rm -f $(EXECUTABLE)
	@echo "> Cleaned output directory."

.PHONY: all run clean
