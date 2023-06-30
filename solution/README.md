# Como executar o verificador de tipos/interpretador?

Por questões de completude, vamos documentar aqui os passos que fizemos para executar as partes do trabalho. Além disso, estamos enviando todo o ambiente utilizado durante o desenvolvimento do trabalho, junto também dos testes e de outros arquivos auxiliares, para que o ambiente usado seja facilmente replicado durante a correção.

Os arquivos PlcChecker.sml, PlcInterp.sml e Plc.sml foram devidamente implementados e, para testá-los fizemos dois arquivos, baseados nos arquivos de testes também disponibilizados do trabalho, para automatizar o trabalho. Esses arquivos são: autoTest.sml e automatedTests.sml.

O primeiro executa um único teste e apresenta seu resultado no terminal, sendo possível passar um programa como texto ou arquivo de extensão .plc. Já o segundo foi baseado no arquivo fullTest.sml e executa a bateria de testes presente no arquivo testCases.sml. Ele cria como saída dois arquivos que apresentam a saída dos testes que possuem falhas e dos que não. Os testes podem ser comparados com o resultado esperado, que também foi disponibilizado, que está presente nos arquivos que possuem o prefixo "Expected-".

Por fim, para executar os testes, basta, executar o comando `sml autoTest.sml` ou `sml automatedTest.sml` no terminal e verificar a saída padrão ou arquivo que foi escrito.

Qualquer dúvida ou pendência estamos a disposição para resolvê-la!
