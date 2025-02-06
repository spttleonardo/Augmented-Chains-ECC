#!/bin/bash
#
# Script de exemplo para submeter trabalho que não use MPI 
#
#SBATCH --job-name=x4  	   # Nome do trabalho a ser executado (para melhor identificação)
#SBATCH --partition=open_cpu               # Em qual fila o trabalho será executado (ver filas disponíveis com o comando sinfo)
#SBATCH --nodes 1                          # Número de nós (computadores) que serão utilizados (1 para códigos openMP)
#SBATCH --cpus-per-task=20                 # Número de cores que será utilizado
#SBATCH --mem 120000                     # Quanto de memória em MB por nó (computador) o programa necessitará. 
#SBATCH --time=10-00:00                    # Tempo máximo de simulação (D-HH:MM). O tempo 00:00:00 corresponde a sem limite.
#SBATCH -o slurm.%N.%j.out                 # Nome do arquivo onde a saída (stdout) será gravada %N = Máquina , %j = Número do trabalho. 
#SBATCH -e slurm.%N.%j.err                 # Nome do arquivo para qual a saída de erros  (stderr) será redirecionada.
#SBATCH --mail-user=maurisones@gmail.com   # Email para enviar notificações sobre alteração no estados do trabalho
#SBATCH --mail-type=BEGIN                  # Envia email quando o trabalho for iniciado
#SBATCH --mail-type=END                    # Envia email quando o trabalho finalizar
#SBATCH --mail-type=FAIL                   # Envia email caso o trabalho apresentar uma mensagem de erro.

# Repassa ao OpenMP quantos cores serão utilizados 
if [ -n "$SLURM_CPUS_PER_TASK" ]; then
    omp_threads=$SLURM_CPUS_PER_TASK
else
    omp_threads=1
fi
export OMP_NUM_THREADS=$omp_threads

# Comando a ser executado. Caso o código necessite o uso de um módulo durante a execução (como o uso de bibliotecas dinâmicas) que não seja carregado automaticamente, incluir o carregamento do mesmo.
#module load python3/3.6.9
#module load cuda80/toolkit/8.0.61
#module load cuda80/blas/8.0.61    
#module load cuda80/fft/8.0.61     
#module load cuda80/nsight/8.0.61  
#module load cuda80/profiler/8.0.61
module load gcc/8.2.0
module load jdk/1.8.0_241-b07



ds=("yeast:0" "birds:0" "enron:0" "EukaryotePseAAC:0" "foodtruck:0" "genbase:0" "HumanGO:0" "HumanPseAAC:0" "medical:0" "PlantGO:0" "PlantPseAAC:0" "slashdot:0" "ohsumed:0" "tmc2007_500:0" "cal500:30" "langlog:10" "ng20:10" "stackex_chess:0" "reutersk500:10") 

#ds=("mediamill:0" "corel5k:0" "bibtex:0"  "stackex_chemistry:0" "stackex_chess:0" "stackex_coffee:0" "stackex_cs:0" "stackex_philosophy:0")

folds=("1" "2" "3")
folds=("4")

cs=("ECC" "MLTB" "XMLTB")

nfactors=("0.3" "0.5" "0.8" "1.0" "1.5" "2.0")

maxpar=8
cores=2

for fold in ${folds[*]}; do
for nf in ${nfactors[*]}; do
	
	for dd in ${ds[*]}; do
	for c in ${cs[*]}; do
		d=`echo $dd|cut -d: -f1`
  		minSupportConcetps=`echo $dd|cut -d: -f2`
                echo "=== iniciando $d$fold"
              	train_file="/home/mauriferrandin/mlds/5-fold/${d}/${d}_train_${fold}"
                test_file="/home/mauriferrandin/mlds/5-fold/${d}/${d}_test_${fold}"
		df
		free -g                                       
		/home/mauriferrandin/R/R-4.0.4/bin/Rscript script_leonardo.R $train_file $test_file $nf $c $cores > "log-${d}-fold-${fold}-${c}-${nf}.out" 2>&1 &
					
		sleep 15
        	par=`ps auxwww|grep script_leonardo.R|grep -v grep|wc -l`
		echo "par: $par"
		while [ "$par" -ge "$maxpar" ];do
			echo "`date` `uptime |cut -d" " -f10-15`  `free|grep Mem`"
	    		sleep 15
			par=`ps auxwww|grep script_leonardo.R|grep -v grep|wc -l`
			df
			free -g
			echo "================"
              	done
        done
	done
done
done

wait


