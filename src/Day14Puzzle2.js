

import readline from 'node:readline';

export function questionImpl(txt) { 
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  return new Promise((resolve, reject) => {
    rl.question(txt, (answer) => {
      resolve(answer);
      rl.close();
    });
  });
}
