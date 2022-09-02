class Client {
    constructor() { }
    async #get(url: string) {
        return fetch(url, {
            method: 'GET',
        });
    }
    async #post(url: string, body: any) {
        return fetch(url, {
            method: 'POST',
            body: JSON.stringify(body),
            headers: new Headers({
                'Content-Type': 'application/json'
            })
        });
    }
    async getBoards(): Promise<Array<any>> {
        let response = await this.#get('http://localhost:8000/games/');
        let json = await response.json();
        return json
    }
    async createNewBoard(size: number): Promise<number> {
        let response = await this.#post('http://localhost:8000/games/', { size });
        let json = await response.json();
        return json['id'];
    }
    async getBoard(id: number) {
        let response = await this.#get(`http://localhost:8000/games/${id}/`);
        let json = await response.json();
        return json;
    }
    async playStone(id:number, x: number, y: number) {
        let response = await this.#post(`http://localhost:8000/games/${id}/play/`, { x, y });
        return await response.json();
    }
}

export default Client;