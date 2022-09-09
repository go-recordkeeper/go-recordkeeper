interface User {
    id: number,
    username: string,
    email: string,
}


export type Ruleset = 'AGA' | 'JPN' | 'CHN';

export interface Record {
    id: number,
    owner: number,
    board_size: number,
    created: string,
    name: string | null,
    black_player: string,
    white_player: string,
    comment: string,
    handicap: number,
    komi: number,
    ruleset: Ruleset,
}

export interface CreateRecordRequest {
    board_size: number,
    name: string | null,
    black_player: string,
    white_player: string,
    comment: string,
    handicap: number,
    komi: number,
    ruleset: Ruleset
}

export interface RecordDetail {
    id: number,
    owner: number,
    board_size: number,
    stones: any[],
}

class Client {
    constructor() { }
    #getToken() {
        return localStorage.getItem('token');
    }
    #setToken(token: string) {
        localStorage.setItem('token', token);
    }
    #deleteToken() {
        localStorage.removeItem('token');
    }
    #headers() {
        let token = this.#getToken();
        let headers = new Headers({});
        if (token) {
            headers.append('Authorization', `Bearer ${token}`);
        }
        return headers;
    }
    async #get(url: string) {
        return fetch(url, {
            method: 'GET',
            headers: this.#headers(),
        });
    }
    async #delete(url: string) {
        return fetch(url, {
            method: 'DELETE',
            headers: this.#headers(),
        });
    }
    async #post(url: string, body?: any) {
        let headers = this.#headers();
        let request: RequestInit = { method: 'POST', headers };
        if (body) {
            request['body'] = JSON.stringify(body);
            headers.append('Content-Type', 'application/json');
        }
        return fetch(url, request);
    }
    async login(username: string, password: string) {
        let response = await this.#post('http://localhost:8000/login/', { username, password });
        if (response.status != 200) {
            throw 'Bad credentials';
        }
        let token = await response.json();
        this.#setToken(token);
    }
    async logout() {
        this.#deleteToken();
    }
    async register(username: string, email: string, password: string) {
        let response = await this.#post('http://localhost:8000/register/', { username, email, password });
        if (response.status != 201) {
            throw 'Couldnt register';
        }
        return await this.login(username, password);
    }
    async getCurrentUser(): Promise<User> {
        let response = await this.#get('http://localhost:8000/user/');
        if (response.status != 200) {
            this.#deleteToken();
            throw 'Not logged in';
        }
        let user = await response.json();
        return user;
    }
    async getRecords(): Promise<Array<Record>> {
        let response = await this.#get('http://localhost:8000/records/');
        let json = await response.json();
        return json
    }
    async createNewRecord(request: CreateRecordRequest): Promise<number> {
        let response = await this.#post('http://localhost:8000/records/', request);
        let json = await response.json();
        return json['id'];
    }
    async deleteRecord(id: number) {
        await this.#delete(`http://localhost:8000/records/${id}/`);
    }
    async getRecord(id: number): Promise<RecordDetail> {
        let response = await this.#get(`http://localhost:8000/records/${id}/`);
        let json = await response.json();
        return json;
    }
    async playStone(id: number, x: number, y: number) {
        let response = await this.#post(`http://localhost:8000/records/${id}/play/`, { x, y });
        return await response.json();
    }
    async undo(id: number) {
        let response = await this.#post(`http://localhost:8000/records/${id}/undo/`);
        return await response.json();
    }
    async pass(id: number) {
        await this.#post(`http://localhost:8000/records/${id}/pass/`);
    }
    async downloadRecord(id: number) {
        let response = await this.#get(`http://localhost:8000/records/${id}/download/`)
        let blob = await response.blob();
        var urlObject = window.URL.createObjectURL(blob);
        var a = document.createElement('a');
        a.href = urlObject;
        let contentDisposition = response.headers.get('content-disposition') as string;
        let filename = (contentDisposition.match(/filename="(.*)"/) as RegExpMatchArray)[1];
        a.download = filename;
        document.body.appendChild(a);
        a.click();
        a.remove();
    }
}

export default Client;
export type { User };