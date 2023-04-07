import { ref, type Ref } from "vue";

interface User {
    id: number,
    username: string,
    email: string,
}


export type Ruleset = 'AGA' | 'JPN' | 'CHN';
export type Winner = 'B' | 'W' | 'U';

export interface Record {
    id: number,
    owner: number,
    board_size: number,
    created: string,
    name: string,
    black_player: string,
    white_player: string,
    comment: string,
    handicap: number,
    komi: number,
    ruleset: Ruleset,
    winner: Winner,
}

export interface RecordDetail extends Record {
    stones: { x: number, y: number, color: 'B' | 'W' }[],
    moves: { position: number | null, color: 'B' | 'W', captures: { x: number, y: number }[] }[],
}

export class APIResponse<T, E> {
    #json: T | undefined;
    #error: E | undefined;
    constructor(arg: { json?: T, error?: E }) {
        let { json, error } = arg;
        this.#json = json;
        this.#error = error;
    }
    is_ok() {
        return this.#json !== undefined;
    }
    is_err() {
        return this.#error !== undefined;
    }
    json(): T {
        return this.#json as T;
    }
    error(): E {
        return this.#error as E;
    }
}

export interface UserAuthError {
    username?: string[],
    email?: string[],
    password?: string[],
    authFailed?: boolean,
}

export interface UpdateRecordRequest {
    name: string | null,
    black_player: string,
    white_player: string,
    comment: string,
    handicap: number,
    komi: number,
    ruleset: Ruleset,
    winner: Winner,
}

export interface CreateRecordRequest {
    board_size: number,
    name: string | null,
    black_player: string,
    white_player: string,
    comment: string,
    handicap: number,
    komi: number,
    ruleset: Ruleset,
}

export interface RecordError {
    black_player?: string[],
    white_player?: string[],
    komi?: string[],
    handicap?: string[],
}

class Client {
    baseUrlTemplate: string;
    implementations: string[];
    constructor() {
        this.baseUrlTemplate = import.meta.env.VITE_API_URL;
        this.implementations = [
          'django',
          'fastapi',
          'haskell',
          'rust',
        ]
    }
    getImplementation() {
        return localStorage.getItem('implementation') || this.implementations[0];
    }
    setImplementation(implementation: string) {
        localStorage.setItem('implementation', implementation);
    }
    #getBaseUrl() {
        return this.baseUrlTemplate.replace('{language}', this.getImplementation()); // TODO language selector
    }
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
    async #get(endpoint: string) {
        return fetch(`${this.#getBaseUrl()}${endpoint}/`, {
            method: 'GET',
            headers: this.#headers(),
        });
    }
    async #delete(endpoint: string) {
        return fetch(`${this.#getBaseUrl()}${endpoint}/`, {
            method: 'DELETE',
            headers: this.#headers(),
        });
    }
    async #post(endpoint: string, body?: any) {
        let headers = this.#headers();
        let request: RequestInit = { method: 'POST', headers };
        if (body) {
            request['body'] = JSON.stringify(body);
            headers.append('Content-Type', 'application/json');
        }
        return fetch(`${this.#getBaseUrl()}${endpoint}/`, request);
    }
    async #put(endpoint: string, body?: any) {
        let headers = this.#headers();
        let request: RequestInit = { method: 'PUT', headers };
        if (body) {
            request['body'] = JSON.stringify(body);
            headers.append('Content-Type', 'application/json');
        }
        return fetch(`${this.#getBaseUrl()}${endpoint}/`, request);
    }
    async initializeUser() {
        try {
            let currentUser = await this.getCurrentUser();
            user.value = currentUser;
        } catch {
            user.value = null;
        }
    }
    async login(username: string, password: string): Promise<APIResponse<User, UserAuthError>> {
        let response = await this.#post('login', { username, password });
        if (response.status == 400) {
            let json = await response.json();
            return new APIResponse({ error: json });
        } else if (response.status == 401) {
            return new APIResponse({ error: { authFailed: true } });
        }
        let token = await response.json();
        this.#setToken(token);
        user.value = await this.getCurrentUser();
        return new APIResponse({ json: user.value });
    }
    async logout() {
        this.#deleteToken();
        user.value = null;
    }
    async register(username: string, email: string, password: string): Promise<APIResponse<User, UserAuthError>> {
        let response = await this.#post('register', { username, email, password });
        if (response.status == 400) {
            let json = await response.json();
            return new APIResponse({ error: json });
        }
        return await this.login(username, password);
    }
    async getCurrentUser(): Promise<User> {
        let response = await this.#get('user');
        if (response.status != 200) {
            this.#deleteToken();
            throw 'Not logged in';
        }
        return await response.json();
    }
    async getRecords(): Promise<Array<Record>> {
        let response = await this.#get('records');
        let json = await response.json();
        return json
    }
    async createNewRecord(request: CreateRecordRequest): Promise<APIResponse<Record, RecordError>> {
        let response = await this.#post('records', request);
        let json = await response.json();
        if (response.status === 400) {
            return new APIResponse({ error: json });
        }
        return new APIResponse({ json });
    }
    async updateRecord(id: number, request: UpdateRecordRequest): Promise<APIResponse<Record, RecordError>> {
        let response = await this.#put(`records/${id}`, request);
        let json = await response.json();
        if (response.status == 400) {
            return new APIResponse({ error: json });
        }
        return new APIResponse({ json });
    }
    async deleteRecord(id: number) {
        await this.#delete(`records/${id}`);
    }
    async getRecord(id: number): Promise<RecordDetail> {
        let response = await this.#get(`records/${id}`);
        let json = await response.json();
        return json;
    }
    async playStone(id: number, x: number, y: number) {
        let response = await this.#post(`records/${id}/play`, { x, y });
        return await response.json();
    }
    async undo(id: number) {
        let response = await this.#post(`records/${id}/undo`);
        return await response.json();
    }
    async pass(id: number) {
        await this.#post(`records/${id}/pass`);
    }
    async downloadRecord(id: number) {
        let response = await this.#get(`records/${id}/download`)
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

let user: Ref<User | null> = ref(null);

export default Client;
export { user };
export type { User };
