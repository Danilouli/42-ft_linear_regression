const csv = require('csvtojson')

const sumarr = arr => arr.reduce((acc,curr) => acc + curr, 0);

csv()
.fromFile('./data.csv')
.then((j)=>{
    j = j.map(({km,price}) => ({km : parseInt(km), price : parseInt(price)}));
    const kms = j.map(({km}) => km);
    const maxKm = Math.max(...kms);
    const minKm = Math.min(...kms);
    const scale = maxKm - minKm;
    j = j.map(({km,price}) => ({km : (km - minKm)/scale, price}));
    const m = j.length;
    let t0T = [0];
    let t1T = [0];
    const t0 = function (n) {
        if (n == 0)
            return 0;
        else {
            const t0n_1 = t0T[n-1] || t0(n-1);
            const t1n_1 = t1T[n-1] || t1(n-1);
            const sum = sumarr(j.map(({km,price}) => (t0n_1 + t1n_1*km - price)));
            const ret = t0n_1 - sum/m;
            t0T[n] = ret;
            return ret;
        }
    };
    const t1 = function (n) {
        if (n == 0)
            return 0;
        else {
            const t0n_1 = t0T[n-1] || t0(n-1);
            const t1n_1 = t1T[n-1] || t1(n-1);
            const sum = sumarr(j.map(({km,price}) => (t0n_1 + t1n_1*km - price)*km));
            const ret = t1n_1 - sum/m;
            t1T[n] = ret;
            return ret;
        }
    };
    setImmediate(() => {
        console.log(t0(8000),t1(8000)/scale);
    });
});
