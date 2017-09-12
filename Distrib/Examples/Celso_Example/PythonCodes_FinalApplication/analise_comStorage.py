###  # -*- coding: iso-8859-15 -*-
__author__ = 'CelsoRocha'

import win32com.client
from win32com.client import makepy
import sys
from pylab import *
import os
import csv

class DSS(object): #Classe DSS

    def __init__(self, dssFileName):

        # Cria uma nova instância do OpenDSS utilizando Ligações Prematuras (Early Bindings)
        sys.argv = ["makepy", "OpenDSSEngine.DSS"]
        makepy.main()
        self.dssObj = win32com.client.Dispatch("OpenDSSEngine.DSS")

        # Inicializa o OpenDSS
        if self.dssObj.Start(0) == False:
            print "DSS Failed to Start"
        else:
            #Atribui uma variável às interfaces comumentes utilizadas para fácil acesso
            self.dssText = self.dssObj.Text
            self.dssCircuit = self.dssObj.ActiveCircuit
            self.dssSolution = self.dssCircuit.Solution
            self.dssCktElement = self.dssCircuit.ActiveCktElement
            self.dssBus = self.dssCircuit.ActiveBus
            self.dssTransformer = self.dssCircuit.Transformers
            self.dssPV = self.dssCircuit.PVSystems

            #Parâmetros Fixos
            loadmult = 1.3
            pv_penetracao = 0.3
            minutos = [2,4,10]
            for i in range(len(minutos)):
                if i==0:
                    self.min = 2  # duração da janela da média móvel
                elif i==1:
                    self.min = 4
                elif i==2:
                    self.min = 10

                # Limpa a memória do OpenDSS
                self.dssObj.ClearAll()

                # Carrega o circuito Master
                self.dssText.Command = "compile " + dssFileName

                # Salva o diretório atual e acrecenta uma pasta para resultados
                self.OpenDSS_folder_path = os.path.dirname(dssFileName)
                self.results_path = self.OpenDSS_folder_path + "/resultados_teste_perdas_somente_eff/resultsStorage_" + str(self.min) + "_min"

                # Seta um novo caso para fácil identificação de resultados
                self.dssText.Command = "set case=" + "LM_" +str(loadmult) + "_PEN_" + str(pv_penetracao)
                self.ajuste_param(loadmult,pv_penetracao)

                # Resolver o resto passo a passo
                # Primeiramente, adicionar os dados da bateria
                self.dssText.Command = "Redirect Storage.DSS"
                self.dssText.Command = "set DataPath=" + self.results_path

                # Ajusta as configurações da solução
                self.solve_settings()
                OriginalSteps = self.dssSolution.Number
                self.dssSolution.Number = 1

                for stepNumber in range(OriginalSteps):
                    control_iter = 1
                    self.dssSolution.InitSnap()  # Inicializa Contadores

                    while not self.dssSolution.ControlActionsDone:

                        self.dssSolution.SolveNoControl()  # Resolve o fluxo de potência sem controles.
                        # Nesse instante, já tenho acesso à nova potência injetada pelo PV. Portanto, posso atualizar a potência injetada pelas baterias.

                        if control_iter == 1 and self.dssSolution.Hour >=29 and self.dssSolution.Hour<=43: #Considerar os PVs só no segundo dia. Menor tempo de simulação
                            self.atualiza_Pbaterias()
                            self.dssSolution.SolveNoControl()

                        self.dssSolution.SampleControlDevices()  # Amostra os controles e popula a Control Queue com ações dos controladores do OpenDSS

                        if self.dssCircuit.CtrlQueue == 0:
                            break  #Sai do While se não há mais ações de controle para realizar

                        self.dssSolution.DoControlActions()  # Empurra pra fora da Control Queue a primeira ação da lista de controle.. quem decide
                        # se a ação é realizada ou não é o elemento de controle quem decide

                        control_iter += 1
                        if control_iter >= self.dssSolution.MaxControlIterations:
                            print "Numero maximo de iteracoes de controle excedido"
                            iteracao = "Maximo excedido"
                            break

                    self.dssSolution.FinishTimeStep()  #atualiza monitores, elementos que integram e incrementa o passo de tempo
                    self.rastreia_solucao()  #só para ter uma ideia de quanto tempo vai demorar

                self.dssText.Command = "export monitors Substationtap"
                self.dssText.Command = "export monitors Reg2a"
                self.dssText.Command = "export monitors Reg3a"
                self.dssText.Command = "export monitors Reg4a"
                self.dssText.Command = "export monitors Reg4b"
                self.dssText.Command = "export monitors Reg3c"
                self.dssText.Command = "export monitors Reg4c"
                self.dssText.Command = "export monitors medidor_trafo_conexao_tensao"
                self.dssText.Command = "export monitors medidor_trafo_conexao_potencia"
                self.dssText.Command = "export monitors Monitor_Baterias_states"
                self.dssText.Command = "export monitors Monitor_Baterias_P"
                self.dssText.Command = "export monitors medidor_PV1"

                self.dssText.Command = "export monitors SubstationtapV"
                self.dssText.Command = "export monitors Reg2aV"
                self.dssText.Command = "export monitors Reg3aV"
                self.dssText.Command = "export monitors Reg4aV"
                self.dssText.Command = "export monitors Reg4bV"
                self.dssText.Command = "export monitors Reg3cV"
                self.dssText.Command = "export monitors Reg4cV"

    def solve_settings(self):

        self.dssText.Command = "set maxcontroliter=200"
        self.dssText.Command = "set maxiterations=100"
        self.dssText.Command = "set mode = daily"
        self.dssText.Command = "set controlmode = time"
        self.dssSolution.StepSize = 1
        self.dssText.Command = "set number=172800"

    def ajuste_param(self,LM, PV_pen):

        self.dssSolution.LoadMult = LM
        OrigTotalLoad = 3490
        if PV_pen == 0:
            self.dssText.Command = "PVSystem.PV_1.enabled = False"
        else:
            self.dssText.Command = "PVSystem.PV_1.enabled = True"
            self.dssText.Command = "PVSystem.PV_1.Pmpp=" +  str(OrigTotalLoad*LM*PV_pen)

    def atualiza_Pbaterias(self):

        # Inicializa Registrador de Deslocamento para média móvel (Só no segundo dia)
        if self.dssSolution.Hour == 29 and self.dssSolution.Seconds ==0:
            if self.min == 10:
                self.shift_reg = [0] * 600
            elif self.min == 4:
                self.shift_reg = [0] * 240
            elif self.min == 2:
                self.shift_reg = [0] * 120

        self.dssPV.idx = 1 #Settando o PV ativo. (só tem um)

        #lógica da média móvel
        for i in range(len(self.shift_reg) -1):
            self.shift_reg[len(self.shift_reg)-i-1]= self.shift_reg[len(self.shift_reg)-i-2]

        self.shift_reg[0] = self.dssPV.kW   #Insere a potência gerada pelo PV no instante atual, no primeiro elemento do registrador de deslocamento

        P_desejado = sum(self.shift_reg)/len(self.shift_reg)    #Cálculo da Potência desejada
        P_bateria = P_desejado - self.dssPV.kW      #Nova potência a ser injetada pela bateria.

        self.dssCircuit.SetActiveElement("storage.storage_pv1")   #Seta a bateria como elemento ativo

        if P_bateria >0:  #Descarrega
            self.dssCktElement.Properties("state").Val = "discharging"
            self.dssCktElement.Properties("%discharge").Val = P_bateria * 100 / 1000

        elif P_bateria <0: #Carrega
            self.dssCktElement.Properties("state").Val = "charging"
            self.dssCktElement.Properties("%charge").Val = (-1)*P_bateria * 100 / 1000
        else:
            self.dssText.Command = "storage.storage_pv1.state=idling"

    def rastreia_solucao(self):
        if self.dssSolution.Seconds == 0:
            print "Hora =" + str(self.dssSolution.Hour)

if __name__ == '__main__':
    d = DSS(r"C:\Users\User\Desktop\TCC\IEEE123Barras\IEEE123Master.dss") #Coloque aqui o endereço do arquivo Master




