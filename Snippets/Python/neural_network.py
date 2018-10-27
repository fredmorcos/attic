import random

class NN:
        def __init__(self, numInputs,  numOutputs, numLayers, numNodesPerLayer, learnRate=0.25, threshold=1, sigCoef=0.5):
                self.weights = []
                self.nodes = []
                self.lambd = learnRate
                self.a = sigCoef

                self.nodes.append([])
                for x in range(numInputs):
                        self.nodes[0].append([0,0,threshold])
                for x in range(numLayers):
                        self.nodes.append([])
                        for y in range(numNodesPerLayer):
                                self.nodes[x+1].append([0,0,threshold])
                self.nodes.append([])
                for x in range(numOutputs):
                        self.nodes[-1].append([0,0,threshold])

                for x in range(numLayers+3):
                        self.weights.append([])
                for x in range(numInputs):
                        self.weights[0].append(1)
                for x in range(numLayers+1):
                        for y in range(len(self.nodes[x])*len(self.nodes[x+1])):
                                self.weights[x+1].append(self.rand())
                for x in range(numOutputs):
                        self.weights[-1].append(1)

        def rand(self):
                return random.uniform(0,1)

        def sigmoid(self, inpt):
                output = 1/(1+(2.71828183**((-self.a)*inpt)))
                return output

        def sigMyZ(self, nodeLoc):
                d = self.nodes[nodeLoc[0]][nodeLoc[1]][0]
                theta = self.nodes[nodeLoc[0]][nodeLoc[1]][2]
                z = self.sigmoid(d + theta)
                self.nodes[nodeLoc[0]][nodeLoc[1]][0] = z

        def passX(self, nodeLoc):
                if nodeLoc[0]>0:
                        self.sigMyZ(nodeLoc)

                z = self.nodes[nodeLoc[0]][nodeLoc[1]][0]
                lenOfNextLayer = len(self.nodes[nodeLoc[0]+1])
                for nextNode in range(lenOfNextLayer):
                        self.nodes[nodeLoc[0]+1][nextNode][0] += (self.weights[nodeLoc[0]+1][(nodeLoc[1]*lenOfNextLayer)+nextNode]*z)

        def runOnce(self, inpt, returns):
                output = []

                for x in range(len(self.nodes[0])):
                        self.nodes[0][x][0] = inpt[x]

                for x in range(len(self.nodes)-1):
                        for y in range(len(self.nodes[x])):
                                self.passX([x,y])

                for x in range(len(self.nodes[-1])):
                        self.sigMyZ([-1,x])
                        output.append(self.nodes[-1][x][0])

                if returns == 1:
                        return output

        def backProp(self, y):

                for x in range(len(self.nodes[-1])):
                        z = self.nodes[-1][x][0]
                        self.nodes[-1][x][0] = 0
                        self.nodes[-1][x][1] = (z*(1-z)*(y[x]-z))
                        dTheta = self.nodes[-1][x][1]*self.lambd
                        self.nodes[-1][x][2] += dTheta
                        count = 0
                        for weight in range(x,len(self.weights[-2]),len(self.nodes[-1])):
                                self.weights[-2][weight] += (dTheta*self.nodes[-2][count][0])
                                count += 1


                for layer in range(2,len(self.nodes)):
                        lenOfThisLayer = len(self.nodes[(-layer)])
                        lenOfNextLayer = len(self.nodes[(-layer)+1])
                        for currentNode in range(lenOfThisLayer):
                                z = self.nodes[-layer][currentNode][0]
                                self.nodes[-layer][currentNode][0] = 0
                                g = 0
                                for nextNode in range(lenOfNextLayer):
                                        g += (self.nodes[(-layer)+1][nextNode][1]*self.weights[-layer][(currentNode*lenOfNextLayer)+nextNode])
                                self.nodes[-layer][currentNode][1] = (z*(1-z)*g)
                                dTheta = self.nodes[-layer][currentNode][1]*(self.lambd)
                                self.nodes[-layer][currentNode][2] += dTheta
                                count = 0
                                for weight in range(currentNode,len(self.weights[(-layer)-1]),lenOfThisLayer):
                                        self.weights[(-layer)-1][weight] += (dTheta*self.nodes[(-layer)-1][count][0])
                                        count += 1

        def getLayers(self):
                return self.nodes

        def getWeights(self):
                return self.weights
